{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MainUi (loadAssets, mainUI) where

import Card
import Constants
import Control.Exception
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Dynamic
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Graphics.Gloss
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Juicy

data UIException
  = LoadException FilePath
  | InternalUnexpectedPictureType FilePath
  deriving (Show, Typeable)

instance Exception UIException

backgrounds :: NE.NonEmpty FilePath
backgrounds = (assetsGenPath ++ "/forest.png") NE.:| []

gameName = "Pixel Card Wars" -- Card Combat Retro

pictureSize ::
  Picture ->
  Maybe (Int, Int)
pictureSize picture =
  case picture of
    Bitmap bd -> Just $ bitmapSize bd
    BitmapSection rect _ -> Just $ rectSize rect
    whatever -> Nothing

data Assets
  = Assets
      { backgroundPics :: NE.NonEmpty Picture,
        creaturePics :: Map.Map CreatureKind Picture
      }

getOrThrow ::
  (MonadThrow m) =>
  Maybe a ->
  UIException ->
  m a
getOrThrow ma e =
  case ma of
    Nothing -> throw e
    Just a -> return a

loadBackgrounds ::
  (MonadIO m, MonadThrow m) =>
  m (NE.NonEmpty Picture)
loadBackgrounds = do
  let pics = NE.map loadBackground backgrounds
  traverse Prelude.id pics
  where
    loadBackground :: (MonadIO m, MonadThrow m) => FilePath -> m Picture
    loadBackground filepath = do
      maybePic <- liftIO $ loadJuicyPNG filepath
      pic :: Picture <- getOrThrow maybePic $ LoadException filepath
      return pic

loadAssets ::
  (MonadIO m, MonadThrow m) =>
  [Creature UI] ->
  m Assets
loadAssets uiData = do
  bgs <- loadBackgrounds
  return $ Assets bgs _

mainUI ::
  (MonadIO m, MonadThrow m) =>
  m ()
mainUI = do
  maybeBG <- liftIO $ loadJuicyPNG bgPath
  bg <- getOrThrow maybeBG $ LoadException bgPath
  bgSize <- getOrThrow (pictureSize bg) $ InternalUnexpectedPictureType bgPath
  liftIO $ display (InWindow gameName bgSize (0, 0)) white bg
  where
    bgPath :: FilePath = NE.head backgrounds
