module World
  ( World (..),
  )
where

import Board
import Card

data World
  = World
      { board :: Board,
        overlaid :: Maybe (PlayerSpot, CardSpot)
      }
