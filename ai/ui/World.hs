module World
  ( World (..),
  )
where

import Board

newtype World = World {board :: Board}
