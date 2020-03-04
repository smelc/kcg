{-# LANGUAGE ScopedTypeVariables #-}

module Board where

import Card
import qualified Data.Map.Strict as Map

data CardSpot
  = TopLeft
  | Top
  | TopRight
  | BottomLeft
  | Bottom
  | BottomRight

type CardsOnTable = Map.Map CardSpot Creature

data PlayerBoard
  = PlayerBoard
      {
      }

data Board
  = Board
      {
      }
