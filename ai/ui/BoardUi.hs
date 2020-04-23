{-# LANGUAGE ScopedTypeVariables #-}

module BoardUi
  ( cardPixelsOffset,
    IntCoord,
  )
where

import Board
import qualified Data.Map.Strict as Map
import Debug.Trace

cellPixelSize = 24 * 3

-- | The number of cells in the background, horizontally
cellBgWidth = 17

-- | The number of cells in the background, vertically
cellBgHeight = 26

-- | The width of a card, in cells
cellCardWidth = 3

-- | The height of a card, in cells
cellCardHeight = 4

-- | The number of cells from the background's bottom left corner
-- | to the bottom left of the bottom left card
botLeftCellOffset = (3, 3)

-- | The number of cells between cards, horizontally
cellHOffset = 1

-- | The number of cells between cards, vertically
cellVOffset = 1

-- | The number of cells between teams, vertically
cellTeamVOffset = 2

-- TODO Use gloss' Vector type instead

type IntCoord = (Int, Int)

type FloatCoord = (Float, Float)

-- | Shifts a coordinate by x
xshift fc x = (fst fc + x, snd fc)

-- | Shifts a coordinate by y
yshift fc y = (fst fc, snd fc + y)

-- | Shifts a coordinate by (x, y)
shift fc x y = (fst fc + x, snd fc + y)

times fc t = (fst fc * t, snd fc * t)

plus fc1 fc2 = (fst fc1 + fst fc2, snd fc1 + snd fc2)

minus fc1 fc2 = (fst fc1 - fst fc2, snd fc1 - snd fc2)

cellToPixels (x, y) = (x * cellPixelSize, y * cellPixelSize)

-- | The offset of a card, in pixels; from the bottom left of a background
cardPixelsOffset :: PlayerSpot -> CardSpot -> IntCoord
cardPixelsOffset playerSpot cardSpot =
  let base = botLeftCellOffset
      team = case playerSpot of
        PlayerBottom -> (0, 0)
        PlayerTop ->
          ( 0,
            cellCardHeight
              + cellVOffset
              + cellCardHeight
              + cellTeamVOffset
          )
      cell = plus base (plus team $ cardOnlyCellOffset cardSpot)
      result = cellToPixels cell
      bgPixelsWidth = (cellBgWidth * cellPixelSize) `div` 2
      bgPixelsHeight = (cellBgHeight * cellPixelSize) `div` 2
      -- Because images are centered according to the biggest one (the background):
      result' = minus result (bgPixelsWidth, bgPixelsHeight)
      -- Because images are centered according to the biggest one (the background):
      cardHalfPixelsWidth = (cellCardWidth * cellPixelSize) `div` 2
      cardHalfPixelsHeight = (cellCardHeight * cellPixelSize) `div` 2
      result'' = plus result' (cardHalfPixelsWidth, cardHalfPixelsHeight)
   in -- trace ("returning " ++ show result'' ++ " for " ++ show cardSpot) result''
      result''
  where
    xoffset = cellCardWidth + cellHOffset
    yoffset = cellCardHeight + cellVOffset
    cardOnlyCellOffset :: CardSpot -> (Int, Int)
    cardOnlyCellOffset BottomLeft = (0, 0)
    cardOnlyCellOffset Bottom = xshift (0, 0) xoffset
    cardOnlyCellOffset BottomRight = xshift (0, 0) $ xoffset * 2
    cardOnlyCellOffset TopLeft = yshift (cardOnlyCellOffset BottomLeft) yoffset
    cardOnlyCellOffset Top = yshift (cardOnlyCellOffset Bottom) yoffset
    cardOnlyCellOffset TopRight = yshift (cardOnlyCellOffset BottomRight) yoffset
