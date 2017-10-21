#!/usr/bin/env stack
{- stack
    script
    --resolver lts-9.9
    --package ansi-terminal
    --package directory
    --package fsnotify
    --package monad-loops
    --package mtl
    --package process
-}
-- TODO: Watch .nvmrc, package.json, & elm-package.json & re-init on change
-- TODO: Watch stack.yaml, & cabal file & re-init on change
-- TODO: Make a `BuildTarget = Client | Server` type that chooses cwd
--       directory & output function/prefix.
-- TODO: Async Client/Server Dependency Installations?
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (forever, void, when)
import Control.Monad.Loops (whileM_)
import Control.Monad.Reader (ReaderT, runReaderT, asks, MonadIO, liftIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (intercalate, isInfixOf)
import Data.Maybe (fromMaybe)
import GHC.IO.Handle
    ( Handle, BufferMode(LineBuffering), hSetBuffering, hIsEOF, hGetLine
    )
import GHC.IO.Handle.FD (stdout, stderr)
import System.Console.ANSI
    ( Color(..), ColorIntensity(Vivid), ConsoleIntensity(BoldIntensity)
    , ConsoleLayer(Foreground), SGR(..), setSGR
    )
import System.Directory
    ( makeAbsolute, removeDirectoryRecursive, doesFileExist, doesDirectoryExist
    , getHomeDirectory
    )
import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitWith)
import System.FSNotify
    ( Event(..), WatchConfig(..), Debounce(..), eventPath, watchTree
    , withManagerConf, defaultConfig
    )
import System.Process
    ( CreateProcess(..), ProcessHandle, StdStream(..), shell, proc
    , createProcess, waitForProcess, terminateProcess
    )
import Text.Read (readMaybe)


main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    args <- getArgs
    getConfig >>= runReaderT (runCommand args)


runCommand :: [String] -> Script
runCommand args =
    case args of
        [] ->
            clientAndServerWatching
        ["watch"] ->
            clientAndServerWatching
        ["build"] ->
            productionBuild
        ["clean"] -> do
            printInfo "Removing Built Files"
            cleanBuiltFiles
            printSuccess "Clean Completed"
        ["sql"] -> do
            printInfo "Starting SQL Shell"
            runInteractive $ proc "psql" ["sese-website"]
        ["server", "repl"] -> do
            initializeServer
            printInfo "Launching GHCi"
            serverDirectory <- getServerDirectory
            jobCount <- stackJobCount
            runInteractive $
                (proc "stack" ["repl", "--main-is", "sese-website-exe", jobCount])
                { cwd = Just serverDirectory }
        ["client", "repl"] -> do
            initializeClient
            printInfo "Launching Elm REPL"
            clientDirectory <- getClientDirectory
            runInteractive $
                (proc "npm" ["run", "elm", "--", "repl"])
                { cwd = Just clientDirectory }
        _ ->
            liftIO . putStrLn $ intercalate "\n" helpText


helpText :: [String]
helpText =
    [ "./manage.hs: SESE Website Management"
    , ""
    , "Available Commands:"
    , ""
    , "watch        (default) Run the Client & Server in Development Mode,"
    , "                       re-building & restarting if source files change."
    , "build                  Build the Client & Server for Production."
    , "clean                  Remove All Built Files."
    , "sql                    Launch a PostgreSQL REPL."
    , "server repl            Launch a Haskell REPL."
    , "client repl            Launch an Elm REPL."
    ]


-- Script Configuration

type Script =
    ReaderT Config IO ()

data Config =
    Config
        { cClientDirectory :: FilePath
        , cServerDirectory :: FilePath
        , serverProcess :: IORef (Maybe ProcessHandle)
        , coreCount :: Integer
        }

getConfig :: IO Config
getConfig =
    Config
        <$> makeAbsolute "./client/"
        <*> makeAbsolute "./server/"
        <*> newIORef Nothing
        <*> determineCoreCount
    where determineCoreCount = do
            (_, Just outHandle, _, _) <- createProcess $
                (shell "grep -P '^core id\t' /proc/cpuinfo | wc -l")
                    { std_out = CreatePipe }
            fromMaybe 2 . readMaybe <$> hGetLine outHandle


getClientDirectory :: ReaderT Config IO FilePath
getClientDirectory = asks cClientDirectory

getServerDirectory :: ReaderT Config IO FilePath
getServerDirectory = asks cServerDirectory

stackJobCount :: ReaderT Config IO String
stackJobCount = asks (("-j" ++) . show . coreCount)


-- Initialization

initializeServer :: Script
initializeServer = do
    serverDirectory <- getServerDirectory
    jobCount <- stackJobCount
    liftIO $
        installDependency "stack" ["setup", jobCount] serverDirectory
            printServerOutput "GHC"
    liftIO $
        installDependency "stack" ["install", "--only-dependencies", jobCount]
            serverDirectory printServerOutput "Server Dependencies"


initializeClient :: Script
initializeClient = do
    clientDirectory <- getClientDirectory
    liftIO $ do
        hasNvm <- (++ "/.nvm/nvm.sh") <$> getHomeDirectory >>= doesFileExist
        when hasNvm $
            printInfo "Found NVM, Installing Node"
                >> getHandle <$> createProcess
                    (shell "sh -c 'source ~/.nvm/nvm.sh; nvm install'")
                    { cwd = Just clientDirectory }
                >>= waitForProcess
                >>= printExitMessage "Node Installed" "Node Installation Failed"
        installDependency "npm" ["install"] clientDirectory printClientOutput
            "Node Dependencies"
        installDependency "npm" ["run", "elm", "--", "package", "install", "--yes"]
            clientDirectory printClientOutput "Elm Dependencies"


-- Commands

clientAndServerWatching :: Script
clientAndServerWatching = do
    initializeServer >> initializeClient

    printInfo "Starting Client Dev Server"
    clientDirectory <- getClientDirectory
    (_, Just clientStdOut, _, clientHandle) <- liftIO $ createProcess
        (proc "npm" ["run", "watch"]) { cwd = Just clientDirectory, std_out = CreatePipe }
    liftIO $ printClientOutput clientStdOut

    serverDirectory <- getServerDirectory
    jobCount <- stackJobCount
    processRef <- asks serverProcess
    liftIO $ buildAndStartServer serverDirectory processRef jobCount

    liftIO . void . withManagerConf (defaultConfig { confDebounce = Debounce 100 }) $ \mgr -> do
        void $ watchTree
            mgr
            "server"
            (\ev -> not $ "stack-work" `isInfixOf` eventPath ev)
            (rebuildAndServe serverDirectory processRef jobCount)
        forever $ threadDelay 1000000

    liftIO $ readIORef processRef >>= maybe (return ()) terminateProcess
    liftIO $ terminateProcess clientHandle


productionBuild :: Script
productionBuild = do
    initializeServer >> initializeClient
    printInfo "Building Client"
    clientDirectory <- getClientDirectory
    clientResult <- liftIO $
        run "npm" ["run", "build"] clientDirectory printClientOutput
            >>= waitForProcess
    case clientResult of
        ExitSuccess ->
            printSuccess "Client Built"
        _ ->
            printError "Client Build Failed"
                >> liftIO (exitWith clientResult)
    printInfo "Building Server"
    serverDirectory <- getServerDirectory
    jobCount <- stackJobCount
    serverResult <- liftIO $
        run "stack" ["build", "--pedantic", jobCount] serverDirectory printServerOutput
            >>= waitForProcess
    case serverResult of
        ExitSuccess ->
            printSuccess "Server Built"
        _ ->
            printError "Server Build Failed"
            >> liftIO (exitWith serverResult)


buildAndStartServer :: FilePath -> IORef (Maybe ProcessHandle) -> String -> IO ()
buildAndStartServer serverDirectory processRef jobCount = do
    printInfo "Building Server"
    buildResult <- run "stack" ["build", "--pedantic", jobCount] serverDirectory printServerOutput
        >>= waitForProcess
    case buildResult of
        ExitSuccess -> do
            printSuccess "Server Build Completed"
            printInfo "Starting Server"
            run "stack" ["exec", "sese-website-exe"] serverDirectory printServerOutput
                >>= writeIORef processRef . Just
            printSuccess "Server Started"
        _ ->
            writeIORef processRef Nothing
                >> printError "Server Build Failure"


rebuildAndServe :: String -> IORef (Maybe ProcessHandle) -> String -> Event -> IO ()
rebuildAndServe serverDirectory processRef jobCount event =
    case event of
        Added _ _ -> do
            printInfo "Rebuild Triggered"
            maybeProcessHandle <- readIORef processRef
            case maybeProcessHandle of
                Just processHandle ->
                    printInfo "Killing Server" >>
                    terminateProcess processHandle
                Nothing ->
                    printInfo "Server Not Running, Process Not Killed"
            buildAndStartServer serverDirectory processRef jobCount
        _ ->
            return ()


cleanBuiltFiles :: Script
cleanBuiltFiles = do
    clientDirectory <- getClientDirectory
    serverDirectory <- getServerDirectory
    liftIO $ do
        remove $ clientDirectory ++ "/dist"
        remove $ clientDirectory ++ "/elm-stuff"
        remove $ clientDirectory ++ "/node_modules"
        remove $ serverDirectory ++ "/.stack-work"
    where remove dir =
            doesDirectoryExist dir >>= flip when (removeDirectoryRecursive dir)


-- Running Processes

run :: FilePath -> [String] -> FilePath -> (Handle -> IO ()) -> IO ProcessHandle
run cmd args dir outputHandler = do
    (_, Just outputHandle, Just errorHandle, processHandle) <- createProcess
        (proc cmd args) { cwd = Just dir, std_out = CreatePipe, std_err = CreatePipe }
    outputHandler outputHandle
    outputHandler errorHandle
    return processHandle

runAndExit :: CreateProcess -> Script
runAndExit cmd = liftIO $
    getHandle <$> createProcess cmd
        >>= waitForProcess
        >>= exitWith

runInteractive :: CreateProcess -> Script
runInteractive cmd =
    runAndExit $ cmd { delegate_ctlc = True }

installDependency :: FilePath -> [String] -> FilePath -> (Handle -> IO ()) -> String -> IO ()
installDependency cmd args dir outputHandler description =
    printInfo ("Installing " ++ description)
        >> run cmd args dir outputHandler
        >>= waitForProcess
        >>= printExitMessage (description ++ " Installed")
                (description ++ "Installation Failed")

getHandle :: (a, b, c, d) -> d
getHandle (_, _, _, handle) = handle


-- Output Utilities

printInfo :: MonadIO m => String -> m ()
printInfo text = liftIO $
    printInBrackets "INFO" Blue
    >> putStrLn ("   " ++ text)

printSuccess :: MonadIO m => String -> m ()
printSuccess text = liftIO $
    printInBrackets "SUCCESS" Green
    >> putStrLn text

printError :: MonadIO m => String -> m ()
printError text = liftIO $
    printInBrackets "FAILED" Red
    >> putStrLn (" " ++ text)

printExitMessage :: String -> String -> ExitCode -> IO ()
printExitMessage successMsg failureMsg status =
    case status of
        ExitSuccess ->
            printSuccess successMsg
        _ ->
            printError failureMsg

printClientOutput :: Handle -> IO ()
printClientOutput =
    prependOutput "CLIENT" Cyan

printServerOutput :: Handle -> IO ()
printServerOutput =
    prependOutput "SERVER" Magenta

prependOutput :: String -> Color -> Handle -> IO ()
prependOutput text color processOutput =
    void . forkIO . whileM_ (not <$> hIsEOF processOutput) $ do
        str <- hGetLine processOutput
        printInBrackets text color
        putStrLn $ " " ++ str

printInBrackets :: String -> Color -> IO ()
printInBrackets text sgrColor =
    putStr "["
        >> setSGR [ SetConsoleIntensity BoldIntensity
                  , SetColor Foreground Vivid sgrColor
                  ]
        >> putStr text
        >> setSGR [Reset]
        >> putStr "] "
