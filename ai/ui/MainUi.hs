{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MainUi where

import Card
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

assets_gen_path = "assets-gen"

backgrounds :: NE.NonEmpty FilePath
backgrounds = (assets_gen_path ++ "/forest.png") NE.:| []

gameName = "Pixel Card Wars" -- Card Combat Retro

pictureSize ::
  Picture ->
  Maybe (Int, Int)
pictureSize picture =
  case picture of
    Bitmap bd -> Just $ bitmapSize bd
    BitmapSection rect _ -> Just $ rectSize rect
    whatever -> Nothing

getOrThrow ::
  (MonadThrow m) =>
  Maybe a ->
  UIException ->
  m a
getOrThrow ma e =
  case ma of
    Nothing -> throw e
    Just a -> return a

loadAssets ::
  [Creature UI] ->
  Map.Map CreatureID Picture
loadAssets uiData = undefined

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
