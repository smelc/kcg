{-# LANGUAGE ScopedTypeVariables #-}

module Event where

import Debug.Trace (trace)
import Graphics.Gloss
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Interface.IO.Interact
import Text.Printf (printf)
import UtilUi

-- | Whether an event is in a picture, given the picture's center
isEventInPicture :: Event -> Picture -> (Float, Float) -> Bool
isEventInPicture event picture (picx, picy) =
  let (width, height) = pictureSize picture
   in case event of
        EventKey _ _ _ (ex, ey) ->
          isPointIn (ex, ey) (width, height) (picx, picy)
        EventMotion (ex, ey) ->
          isPointIn (ex, ey) (width, height) (picx, picy)
        EventResize _ -> False

isEventIn :: Event -> (Float, Float) -> (Float, Float) -> Bool
isEventIn event (rectWidth, rectHeight) (rectCenterx, rectCentery) =
  case event of
    EventKey _ _ _ (ex, ey) ->
      isPointIn (ex, ey) (rectWidth, rectHeight) (rectCenterx, rectCentery)
    EventMotion (ex, ey) ->
      isPointIn (ex, ey) (rectWidth, rectHeight) (rectCenterx, rectCentery)
    EventResize _ -> False

-- | Whether a point is in a rectangle, given the rectangle's center
isPointIn ::
  (Ord a1, Ord a2, Fractional a1, Fractional a2, Show a1, Show a2) =>
  (a1, a2) ->
  (a1, a2) ->
  (a1, a2) ->
  Bool
isPointIn (x, y) (rectWidth, rectHeight) (rectCenterx, rectCentery) =
  let result =
        (rectCenterx - (rectWidth / 2) <= x && x <= rectCenterx + (rectWidth / 2))
          && (rectCentery - (rectHeight / 2) <= y && y <= rectCentery + (rectHeight / 2))
   in trace (printf "Is %s in rectangle %s centered at %s -> %s" (show (x, y)) (show (rectWidth, rectHeight)) (show (rectCenterx, rectCentery))) result
