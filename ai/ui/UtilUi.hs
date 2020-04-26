module UtilUi where

import Card
import Control.Exception (Exception, throw)
import Data.Tuple.Extra (both)
import Data.Typeable (Typeable)
import GHC.Stack (HasCallStack)
import Graphics.Gloss
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Interface.IO.Interact
import Graphics.Gloss.Juicy
import Numeric.Extra (intToFloat)

data UIException
  = LoadException FilePath
  | CreaturePictNotFoundException CreatureID
  | CreatureLoadException FilePath CreatureID
  | InternalUnexpectedPictureType Picture
  deriving (Show, Typeable)

instance Exception UIException

pictureSize ::
  HasCallStack =>
  Picture ->
  (Float, Float)
pictureSize picture =
  case picture of
    Bitmap bd -> both intToFloat $ bitmapSize bd
    BitmapSection rect _ -> both intToFloat $ rectSize rect
    Pictures subs -> do
      let subSizes = map pictureSize subs
      foldr sizeMax (0, 0) subSizes
      where
        sizeMax (i1, i2) (j1, j2) = (max i1 j1, max i2 j2)
    Scale xmult ymult pic -> do
      let (subx, suby) = pictureSize pic
      (xmult * subx, ymult * suby)
    Translate x y pic -> do
      let (subx, suby) = pictureSize pic
      (x + subx, y + suby)
    whatever -> throw $ InternalUnexpectedPictureType picture
