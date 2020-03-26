{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Board where

import Card
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

data CardSpot
  = TopLeft
  | Top
  | TopRight
  | BottomLeft
  | Bottom
  | BottomRight

type CardsOnTable = Map.Map CardSpot (Creature Core)

type CardsInHand = Set.Set (Card Core)

data PlayerPart
  = PlayerPart
      { visible :: CardsOnTable,
        invisible :: CardsInHand
      }

data Board
  = Board
      { player1 :: PlayerPart,
        player2 :: PlayerPart
      }
