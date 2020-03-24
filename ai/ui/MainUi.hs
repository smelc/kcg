{-# LANGUAGE ScopedTypeVariables #-}

module MainUi where

import Control.Exception
import Data.Dynamic
import Graphics.Gloss
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Juicy

backgrounds = ["assets-gen/forest.png"]

data UIException
  = LoadException FilePath
  | InternalUnexpectedPictureType FilePath
  deriving (Show, Typeable)

instance Exception UIException

gameName = "Pixel Card Wars" -- Card Combat Retro

pictureSize ::
  Picture ->
  Maybe (Int, Int)
pictureSize picture =
  case picture of
    Bitmap bd -> Just $ bitmapSize bd
    BitmapSection rect _ -> Just $ rectSize rect
    whatever -> Nothing

mainUI :: IO ()
mainUI = do
  maybeBG <- loadJuicyPNG bgPath
  let bg =
        case maybeBG of
          Nothing -> throw $ LoadException bgPath
          Just (value :: Picture) -> value
  let bgSize =
        case pictureSize bg of
          Nothing -> throw $ InternalUnexpectedPictureType bgPath
          Just size -> size
  display (InWindow gameName bgSize (0, 0)) white bg
  where
    bgPath :: FilePath = backgrounds !! 0
