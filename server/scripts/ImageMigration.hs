{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-| This script is used for migrating Product & Category images from our
old ZenCart site to the new site's structure. It uses the new site's
database for fetching each Product's image, so it is expected that you have
run the `data-migration` script first.

Images are transfered from the ZenCart media folder, hashed, scaled,
optimized, and arranged in the structure expected by the new site.

-}
import Control.Monad (filterM)
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Reader (MonadTrans, lift)
import Data.Foldable (asum)
import Data.Monoid ((<>))
import Data.Pool (destroyAllResources)
import Database.Persist.Postgresql
    ( Entity(..), ConnectionPool, SqlWriteT, (!=.), (=.), createPostgresqlPool
    , runSqlPool, selectList, update
    )
import System.Directory
    ( doesFileExist, createDirectoryIfMissing, doesDirectoryExist, listDirectory )
import System.FilePath ((</>), splitExtension, takeFileName)

import Images (ImageConfig, makeImageConfig, scaleImage)
import Models (Category(..), Product(..), EntityField(..))

import qualified Data.ByteString as BS
import qualified Data.Text as T


outputDirectory :: T.Text
outputDirectory = "media/"

oldDirectory :: String
oldDirectory = "old_media/"

largeDirectory :: String
largeDirectory = "large/"

largeExt :: String
largeExt = "_LRG"

mediumDirectory :: String
mediumDirectory = "medium/"

mediumExt :: String
mediumExt = "_MED"


-- | Transfer images from the old media folder to the new folder
-- & structure while scaling and optimizing them.
main :: IO ()
main = do
    cfg <- makeImageConfig
    createDirectories
    searchPaths <- getDirectoriesRecursive "old_media"
    psqlConn <- connectToPostgres
    flip runSqlPool psqlConn $ do
        selectList [ProductImageUrl !=. ""] []
            >>= mapM_ (migrateProductImage cfg searchPaths)
        selectList [CategoryImageUrl !=. ""] []
            >>= mapM_ (migrateCategoryImage cfg searchPaths)
    destroyAllResources psqlConn


-- | Connect to the new site's database.
connectToPostgres :: IO ConnectionPool
connectToPostgres =
    runNoLoggingT $ createPostgresqlPool "dbname=sese-website" 4


-- | Create the directories for Product & Category images.
createDirectories :: IO ()
createDirectories =
    createDirectoryIfMissing True (T.unpack outputDirectory ++ "products/")
        >> createDirectoryIfMissing True (T.unpack outputDirectory ++ "categories/")


-- | Migrate a Product image by scaling it & saving the new hashed name.
migrateProductImage :: ImageConfig -> [FilePath] -> Entity Product -> SqlWriteT IO ()
migrateProductImage cfg searchPaths (Entity pId prod) = do
    let oldName = T.unpack $ productImageUrl prod
    maybePath <- asum <$> mapM (findLargestImage . (</> oldName)) searchPaths
    case maybePath of
        Nothing ->
            lift . putStrLn $ "Could not find image: " <> oldName
        Just path -> do
            contents <- lift $ BS.readFile path
            newPath <- lift $ scaleImage cfg (T.pack oldName) (T.unpack $ outputDirectory <> "products/") contents
            let newName = takeFileName newPath
            update pId [ProductImageUrl =. T.pack newName]

-- | Migrate a Category image by scaling it & saving the new hashed name.
migrateCategoryImage :: ImageConfig -> [FilePath] -> Entity Category -> SqlWriteT IO ()
migrateCategoryImage cfg searchPaths (Entity cId category) = do
    let oldName = T.unpack $ categoryImageUrl category
    maybePath <- asum <$> mapM (findLargestImage . (</> oldName)) searchPaths
    case maybePath of
        Nothing ->
            lift . putStrLn $ "Could not find image: " <> oldName
        Just path -> do
            contents <- lift $ BS.readFile path
            newPath <- lift $ scaleImage cfg (T.pack oldName) (T.unpack $ outputDirectory <> "categories/") contents
            let newName = takeFileName newPath
            update cId [CategoryImageUrl =. T.pack newName]


-- | Return all the directories in a root directory, searching recursively.
getDirectoriesRecursive :: FilePath -> IO [FilePath]
getDirectoriesRecursive baseDir = do
    subDirectories <- listDirectory baseDir
        >>= filterM (doesDirectoryExist . (baseDir </>))
    subDirContents <- concat
        <$> mapM (getDirectoriesRecursive . (baseDir </>)) subDirectories
    return $ baseDir : subDirContents


-- | Find the largest version of an un-migrated image by checking the large
-- & medium size folders with the appropriate filename suffixes.
findLargestImage :: (MonadTrans m, Monad (m IO)) => FilePath -> m IO (Maybe FilePath)
findLargestImage productImage =
    let
        (pathAndName, extension) =
            splitExtension productImage
        withoutTopDir = drop 1 $ dropWhile (/= '/') pathAndName
        largeFile =
            oldDirectory </> largeDirectory </> withoutTopDir ++ largeExt ++ extension
        mediumFile =
            oldDirectory </> mediumDirectory </> withoutTopDir ++ mediumExt ++ extension
    in
        find isFile [largeFile, mediumFile, productImage]
    where
        find predicate xs =
            case xs of
                [] ->
                    return Nothing
                x:rest -> do
                    matches <- predicate x
                    if matches then
                        return $ Just x
                    else
                        find predicate rest
        isFile =
            lift . doesFileExist
