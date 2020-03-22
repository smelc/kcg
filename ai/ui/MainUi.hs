{-# LANGUAGE ScopedTypeVariables #-}

module MainUi where

import Control.Exception
import Data.Dynamic
import Graphics.Gloss
import Graphics.Gloss.Juicy

backgrounds = ["assets-gen/forest.png"]

data UIException = LoadException FilePath
  deriving (Show, Typeable)

instance Exception UIException

mainUI :: IO ()
mainUI = do
  maybeBG <- loadJuicyPNG bgPath
  let bg = case maybeBG of
        Nothing -> throw $ LoadException bgPath
        Just (value :: Picture) -> value
  display (InWindow "Nice Window" (640, 640) (0, 0)) white bg
  where
    bgPath :: FilePath = backgrounds !! 0
