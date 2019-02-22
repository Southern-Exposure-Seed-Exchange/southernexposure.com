#!/usr/bin/env stack
{- stack
    script
    --ghc-options -Wall
    --ghc-options -Wcompat
    --ghc-options -Wincomplete-record-updates
    --ghc-options -Wincomplete-uni-patterns
    --ghc-options -Wredundant-constraints
    --resolver lts-10.9
    --package ansi-terminal
    --package directory
    --package fsnotify
    --package monad-loops
    --package mtl
    --package process
    --package time
-}
-- TODO: Watch .nvmrc, package.json, & elm-package.json & re-init on change
-- TODO: Make a `BuildTarget = Client | Server` type that chooses cwd
--       directory & output function/prefix.
-- TODO: Async Client/Server Dependency Installations?
-- TODO: Run haddock & hoogle servers:
--       https://lexi-lambda.github.io/blog/2018/02/10/an-opinionated-guide-to-haskell-in-2018/
-- TODO: Run elm linters(xref, analyse, etc.):
--       https://dev.to/zwilias/elm-tools-571a
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad ((>=>), forever, void, when)
import Control.Monad.Loops (whileM_)
import Control.Monad.Reader (ReaderT, runReaderT, asks, MonadIO, liftIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (intercalate, isInfixOf, isSuffixOf)
import Data.Maybe (fromMaybe)
import Data.Time (UTCTime, getCurrentTime)
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
    ( WatchConfig(..), Debounce(..), eventPath, watchTree
    , withManagerConf, defaultConfig
    )
import System.Process
    ( CreateProcess(..), ProcessHandle, StdStream(..), shell, proc
    , createProcess, waitForProcess, terminateProcess, getProcessExitCode
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

type ProcessData =
    (ProcessHandle, UTCTime)

data Config =
    Config
        { cClientDirectory :: FilePath
        , cServerDirectory :: FilePath
        , serverProcess :: IORef (Maybe ProcessHandle)
        , serverBuildProcess :: IORef (Maybe ProcessData)
        , coreCount :: Integer
        }

getConfig :: IO Config
getConfig =
    Config
        <$> makeAbsolute "./client/"
        <*> makeAbsolute "./server/"
        <*> newIORef Nothing
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
        stackSetup jobCount serverDirectory >>
        installServerDependencies jobCount serverDirectory


initializeClient :: Script
initializeClient = do
    clientDirectory <- getClientDirectory
    liftIO $ do
        hasNvm <- (++ "/.nvm/nvm.sh") <$> getHomeDirectory >>= doesFileExist
        when hasNvm $ do
            printInfo "Found NVM, Installing Node"
            (_, Just nvmOut, Just nvmErr, nvmHandle) <- createProcess
                (shell "sh -c '. ~/.nvm/nvm.sh; nvm install'")
                { cwd = Just clientDirectory
                , std_out = CreatePipe
                , std_err = CreatePipe
                }
            printClientOutput nvmOut >> printClientOutput nvmErr
            waitForProcess nvmHandle >>=
                printExitMessage "Node Installed" "Node Installation Failed"
        installDependency "npm" ["install"] clientDirectory printClientOutput
            "Node Dependencies"


-- Commands

clientAndServerWatching :: Script
clientAndServerWatching = do
    initializeServer >> initializeClient

    printInfo "Starting Client Dev Server"
    clientDirectory <- getClientDirectory
    (_, Just clientStdOut, Just clientStdErr, clientHandle) <- liftIO $
        createProcess
            (proc "npm" ["run", "watch"])
            { cwd = Just clientDirectory
            , std_out = CreatePipe
            , std_err = CreatePipe
            }
    liftIO $ printClientOutput clientStdOut
    liftIO $ printClientOutput clientStdErr

    serverDirectory <- getServerDirectory
    jobCount <- stackJobCount
    buildRef <- asks serverBuildProcess
    serverRef <- asks serverProcess
    liftIO $ buildAndStartServer serverDirectory buildRef serverRef jobCount

    let watchConfig = defaultConfig { confDebounce = Debounce 100 }
    liftIO . void . withManagerConf watchConfig $ \mgr -> do
        void . watchTree mgr serverDirectory isProjectFile $ \event ->
            if ".hs" `isSuffixOf` eventPath event then
                rebuildAndServe serverDirectory buildRef serverRef jobCount
            else
                when (requiresReinitialization event) $ do
                    printInfo "Reinitialization Triggered"
                    stackSetup jobCount serverDirectory
                    installServerDependencies jobCount serverDirectory
                    rebuildAndServe serverDirectory buildRef serverRef jobCount
        forever (threadDelay 1000000)

    liftIO $ readIORef serverRef >>= maybe (return ()) terminateProcess
    liftIO $ terminateProcess clientHandle
    where isProjectFile event =
            not $ "stack-work" `isInfixOf` eventPath event
          requiresReinitialization event =
            any (`isSuffixOf` eventPath event)
                [ "stack.yaml"
                , "package.yaml"
                ]


productionBuild :: Script
productionBuild = do
    cleanBuiltFiles
    initializeClient
    printInfo "Building Client"
    clientDirectory <- getClientDirectory
    liftIO $ run "npm" ["run", "build"] clientDirectory printClientOutput
        >>= exitOnError "Client"
    initializeServer
    printInfo "Building Server"
    serverDirectory <- getServerDirectory
    jobCount <- stackJobCount
    liftIO $ run "stack" ["build", "--pedantic", jobCount, "--color", "always", "--ghc-options", "-O2"]
        serverDirectory printServerOutput
        >>= exitOnError "Server"
    where exitOnError description =
            waitForProcess >=> \status ->
                case status of
                    ExitSuccess ->
                        printSuccess $ description ++ " Built"
                    _ ->
                        printError (description ++ " Build Failed")
                        >> liftIO (exitWith status)


stackSetup :: String -> FilePath -> IO ()
stackSetup jobCount serverDirectory =
    installDependency "stack" ["setup", jobCount, "--color", "always"] serverDirectory
        printServerOutput "GHC"


installServerDependencies :: String -> FilePath -> IO ()
installServerDependencies jobCount serverDirectory =
    installDependency "stack" ["install", "--only-dependencies", jobCount, "--color", "always"]
        serverDirectory printServerOutput "Server Dependencies"


buildAndStartServer :: FilePath -> IORef (Maybe ProcessData) -> IORef (Maybe ProcessHandle) -> String -> IO ()
buildAndStartServer serverDirectory buildRef serverRef jobCount = do
    printInfo "Building Server"
    buildProcess <- run "stack" ["build", "--pedantic", jobCount, "--color", "always"] serverDirectory printServerOutput
    currentTime <- getCurrentTime
    writeIORef buildRef (Just (buildProcess, currentTime))
    buildResult <- waitOrAbort currentTime buildProcess
    case buildResult of
        Just ExitSuccess -> do
            printSuccess "Server Build Completed"
            printInfo "Starting Server"
            run "stack" ["exec", "sese-website-exe"] serverDirectory printServerOutput
                >>= writeIORef serverRef . Just
            printSuccess "Server Started"
        Just _ ->
            writeIORef buildRef Nothing
                >> printError "Server Build Failure"
        Nothing ->
            return ()
    where waitOrAbort processTime processHandle = do
            currentProcessTime <- fmap snd <$> readIORef buildRef
            if currentProcessTime /= Just processTime then
                terminateProcess processHandle >>
                printInfo "Terminated Previous Build" >>
                return Nothing
            else
                threadDelay 100
                    >> getProcessExitCode processHandle
                    >>= maybe (waitOrAbort processTime processHandle)
                        (\code -> writeIORef buildRef Nothing >> return (Just code))


rebuildAndServe :: String -> IORef (Maybe ProcessData) -> IORef (Maybe ProcessHandle) -> String -> IO ()
rebuildAndServe serverDirectory buildRef serverRef jobCount = do
    printInfo "Rebuild Triggered"
    maybeServerHandle <- readIORef serverRef
    case maybeServerHandle of
        Just processHandle ->
            printInfo "Killing Server" >>
            terminateProcess processHandle >>
            writeIORef serverRef Nothing
        Nothing ->
            return ()
    buildAndStartServer serverDirectory buildRef serverRef jobCount


cleanBuiltFiles :: Script
cleanBuiltFiles = do
    clientDirectory <- getClientDirectory
    serverDirectory <- getServerDirectory
    liftIO $ do
        remove $ clientDirectory ++ "/dist"
        remove $ clientDirectory ++ "/elm-stuff"
        remove $ clientDirectory ++ "/node_modules"
        remove $ serverDirectory ++ "/.stack-work"
        remove $ serverDirectory ++ "/dist"
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
