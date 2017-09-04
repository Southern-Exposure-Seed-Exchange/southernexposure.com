{-# LANGUAGE OverloadedStrings #-}
import Utils

import Data.Monoid ((<>))
import Database.MySQL.Base (MySQLConn, Query(..), MySQLValue(..), query_)
import System.FilePath (splitExtension, takeFileName)
import System.Directory (doesPathExist, copyFile, createDirectoryIfMissing)

import qualified Data.Text as T
import qualified System.IO.Streams as Streams


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


main :: IO ()
main = do
    createDirectories
    mysqlConn <- connectToMysql
    productImagePaths <- getProductImagePaths mysqlConn
    mapM_ (findLargestImageAndMove "products/") productImagePaths
    categoryImagePaths <- getCategoryImagePaths mysqlConn
    mapM_ (findLargestImageAndMove "categories/") categoryImagePaths


createDirectories :: IO ()
createDirectories =
    createDirectoryIfMissing True (T.unpack outputDirectory ++ "products/")
        >> createDirectoryIfMissing True (T.unpack outputDirectory ++ "categories/")


getProductImagePaths :: MySQLConn -> IO [T.Text]
getProductImagePaths mysql = do
    (_, productStream) <- query_ mysql . Query $
        "SELECT products_image FROM products WHERE products_image <> \"\""
    map (\[MySQLText path] -> path) <$> Streams.toList productStream


getCategoryImagePaths :: MySQLConn -> IO [T.Text]
getCategoryImagePaths mysql = do
    (_, categoryStream) <- query_ mysql . Query $
        "SELECT categories_image FROM categories WHERE categories_image <> \"NULL\""
    map (\[MySQLText path] -> path) <$> Streams.toList categoryStream


findLargestImageAndMove :: T.Text -> T.Text -> IO ()
findLargestImageAndMove outputParent productImage =
    let
        (pathAndName, extension) =
            splitExtension $ T.unpack productImage
        largeFile =
            largeDirectory ++ pathAndName ++ largeExt ++ extension
        mediumFile =
            mediumDirectory ++ pathAndName ++ mediumExt ++ extension
    in
        find isFile [largeFile, mediumFile, T.unpack productImage]
        >>= maybe (return ()) moveImage
        where find predicate xs =
                  case xs of
                      [] ->
                        return Nothing
                      x:rest -> do
                        matches <- predicate x
                        if matches then
                            return . Just $ T.pack x
                        else
                            find predicate rest
              isFile path =
                  doesPathExist $ oldDirectory ++ path
              moveImage path =
                  copyFile (oldDirectory ++ T.unpack path)
                    $ T.unpack (outputDirectory <> outputParent)
                    ++ takeFileName (T.unpack productImage)
