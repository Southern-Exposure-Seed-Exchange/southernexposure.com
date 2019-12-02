{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-| Contains types & functions for manipulation of image files.

It can be used to generate scaled & optimized versions of uploaded images,
as well as generating JSON data for the @srcset@ attribute of images shown
in the client.

-}
module Images
    (
    -- * Image Configuration / Available Programs
      ImageConfig(..)
    , makeImageConfig
    -- * Image Path to Srcset
    , ImageSourceSet(..)
    , ScaledImageData(..)
    , makeSourceSet
    -- * Manipulation / Scaling
    , scaleImage
    , ImageError(..)
    ) where

import Control.Exception.Safe (Exception, MonadThrow, Typeable, throwM, tryIO)
import Control.Monad (forM_, when, unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, asks)
import Data.Aeson ((.=), ToJSON(..), object)
import Data.Char (toLower)
import Data.Digest.Pure.MD5 (md5)
import Data.Text (Text)
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import System.Directory (findExecutable, createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>), takeBaseName, takeExtension)
import System.Process.Typed (runProcess_, setStdin, setStdout, closed, proc)

import Config (Config, getMediaDirectory)
import Models.Utils (slugify)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T

-- | Defines which image optimizers are available on the current system.
data ImageConfig =
    ImageConfig
        { icOptipngAvailable :: Bool
        -- ^ Is the @optipng@ executable available?
        , icJpegtranAvailable :: Bool
        -- ^ Is the @jpegtran@ executable available?
        , icGifsicleAvailable :: Bool
        -- ^ Is the @gifsicle@ executable available?
        , icGraphicsMagickAvailable :: Bool
        -- ^ Is GraphicsMagick's @mogrify@ executable available?
        }

-- | Build an 'ImageConfig' by testing for the existence of the optimizer
-- executables.
makeImageConfig :: MonadIO m => m ImageConfig
makeImageConfig =
    ImageConfig
        <$> hasExe "optipng"
        <*> hasExe "jpegtran"
        <*> hasExe "gifsicle"
        <*> hasExe "gm"
  where
    hasExe = fmap isJust . liftIO . findExecutable


-- | Contains paths to the original image, as well as any generated scaled
-- images.
data ImageSourceSet
    = ImageSourceSet
        { issOriginal :: Maybe FilePath
        , issExtraSmall :: Maybe ScaledImageData
        , issSmall :: Maybe ScaledImageData
        , issMedium :: Maybe ScaledImageData
        , issLarge :: Maybe ScaledImageData
        , issExtraLarge :: Maybe ScaledImageData
        } deriving (Show, Read, Eq)

instance ToJSON ImageSourceSet where
    toJSON ImageSourceSet {..} =
        object
            [ "original" .= issOriginal
            , "xs" .= issExtraSmall
            , "sm" .= issSmall
            , "md" .= issMedium
            , "lg" .= issLarge
            , "xl" .= issExtraLarge
            ]

-- | Contains a path to a scaled image as well as the width the image was
-- scaled to. Used for generating @srcset@ attributes for @img@ HTML
-- elements.
data ScaledImageData
    = ScaledImageData
        { sidPath :: FilePath
        , sidWidth :: Int
        } deriving (Show, Read, Eq)

instance ToJSON ScaledImageData where
    toJSON ScaledImageData {..} =
        object
            [ "src" .= sidPath
            , "width" .= sidWidth
            ]

-- | Build the ImageSourceSet information for the image with the given
-- filename in the specified base directory.
makeSourceSet :: (MonadReader Config m, MonadIO m) => FilePath -> String -> m ImageSourceSet
makeSourceSet directory filename =
    ImageSourceSet
        <$> pure getOriginal
        <*> getScaledData ExtraSmall
        <*> getScaledData Small
        <*> getScaledData Medium
        <*> getScaledData Large
        <*> getScaledData ExtraLarge
  where
    getOriginal :: Maybe FilePath
    getOriginal =
        if null filename then
            Nothing
        else
            Just $ directory </> "originals" </> filename
    getScaledData :: (MonadReader Config m, MonadIO m) => ImageSize -> m (Maybe ScaledImageData)
    getScaledData size = do
        mediaDirectory <- asks getMediaDirectory
        liftIO $ do
            let scaledPath = directory </> imageDirectory size </> filename
            imageExists <- doesFileExist $ mediaDirectory </> scaledPath
            if imageExists then
                return $ Just $ ScaledImageData scaledPath (sizeWidth size)
            else
                return Nothing

-- | All image sizes that are generated when scaling an image.
data ImageSize
    = ExtraSmall
    | Small
    | Medium
    | Large
    | ExtraLarge
    deriving (Enum, Bounded)

-- | The pixel width for an 'ImageSize'. All scaling will be done to these
-- fixed widths - the scaled height will be calculated to preserve the
-- aspect ratio.
sizeWidth :: ImageSize -> Int
sizeWidth = \case
    ExtraSmall -> 100
    Small -> 200
    Medium -> 400
    Large -> 800
    ExtraLarge -> 1200

-- | Generate the directory name for housing images scaled to the
-- 'ImageSize'.
imageDirectory :: ImageSize -> FilePath
imageDirectory s = "width-" <> show (sizeWidth s)


-- | The image formats that support optimization.
data OptimizableFormat
    = JPEG
    | GIF
    | PNG
    deriving (Eq, Show)


-- | Possible errors while scaling/optimizing images.
data ImageError
    = DirectoryCreationError FilePath IOError
    deriving (Show, Typeable)

instance Exception ImageError


-- | Generate & optimize all scaled versions of an image, given it's
-- filename, base directory path(e.g., @media/products/@, and the image
-- contents as a ByteString.
--
-- The contents will be hashed, the original will be saved in a separate
-- folder, and each scaled version will be saved in the folder specified by
-- the 'imageDirectory' function. Each image will have the original file's
-- hash append to the filename.
--
-- The filepath to the original image will be returned.
--
-- Throws 'ImageError'.
scaleImage :: (MonadIO m, MonadThrow m) => ImageConfig -> Text -> FilePath -> BS.ByteString -> m FilePath
scaleImage config filename destinationDirectory fileContents = do
    let fileHash = md5 $ LBS.fromStrict fileContents
        baseName = takeBaseName $ T.unpack filename
        extension = takeExtension $ T.unpack filename
        newName = T.unpack (slugify $ T.pack baseName) <> "-" <> show fileHash <> extension
        imageFormat = getFormat
    let originalDirectory = destinationDirectory </> "originals"
    createDirectory originalDirectory
    let originalPath = originalDirectory </> newName
    originalExists <- liftIO $ doesFileExist originalPath
    unless originalExists $ do
        liftIO $ BS.writeFile (originalDirectory </> newName) fileContents
        optimizeImage config imageFormat originalPath
    forM_ [minBound .. maxBound] $ \size -> do
        let newWidth = sizeWidth size
            scaledDirectory = destinationDirectory </> imageDirectory size
        createDirectory scaledDirectory
        let scaledPath = scaledDirectory </> newName
        scaledExists <- liftIO $ doesFileExist scaledPath
        unless scaledExists $ do
            resizeWithGraphicsMagick config originalPath scaledDirectory newWidth
            optimizeImage config imageFormat scaledPath
    return originalPath
  where
    -- Use the extension of the filename to determine the optimization
    -- format.
    getFormat =
        case map toLower (takeExtension $ T.unpack filename) of
            ".png" ->
                Just PNG
            ".jpg" ->
                Just JPEG
            ".jpeg" ->
                Just JPEG
            ".gif" ->
                Just GIF
            _ ->
                Nothing
    -- Create a directory & it's parents, rethrowing any IOErrors as
    -- DirectoryCreationErrors.
    createDirectory path =
        liftIO (tryIO $ createDirectoryIfMissing True path) >>=
            either (throwM . DirectoryCreationError path) return


-- | Call GraphicsMagick's @mogrify@ command to resize a file to a new width,
-- placing it in a different output directory.
--
-- Does nothing if imagemagick is not available on the current system.
resizeWithGraphicsMagick :: MonadIO m => ImageConfig -> FilePath -> FilePath -> Int -> m ()
resizeWithGraphicsMagick cfg inputPath outputDirectory targetWidth =
    when (icGraphicsMagickAvailable cfg) $ runProcess_ $
        proc "gm" ["mogrify", "-output-directory", outputDirectory, "-resize", show targetWidth <> "x!", inputPath]


-- | Call an optimization program on the given file, depending on the file
-- format & whether or not the optimization program is available on the
-- system.
optimizeImage :: MonadIO m => ImageConfig -> Maybe OptimizableFormat -> FilePath -> m ()
optimizeImage ImageConfig {..} format path =
    case format of
        Just PNG ->
            runWhen icOptipngAvailable "optipng"
                ["-o7", "-preserve", "-silent", "-strip", "all", path]
        Just JPEG ->
            runWhen icJpegtranAvailable "jpegtran"
                ["-optimize", "-progressive", "-copy", "none", "-outfile", path, path]
        Just GIF ->
            runWhen icGifsicleAvailable "gifsicle" ["-O3", "--batch", path]
        Nothing ->
            return ()
  where
    runWhen predicate program args =
        let processSpec = setStdin closed $ setStdout closed $ proc program args
        in  when predicate $ runProcess_ processSpec
