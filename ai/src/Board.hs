{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Board where

import Card
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- | The spot of a card, as visible from the bottom of the screen. For the
-- | top part, think as if it was in the bottom, turning the board
-- | 180 degrees clockwise
data CardSpot
  = TopLeft
  | Top
  | TopRight
  | BottomLeft
  | Bottom
  | BottomRight
  deriving (Enum, Eq, Ord, Show)

type CardsOnTable = Map.Map CardSpot (Creature Core)

-- | A convenience method for building an instance of CardsOnTable
-- | from a list. First member of the list if TopLeft, then Top, then
-- | TopRight, then BottomLeft, then Bottom, then BottomRight. Maybe
-- | allow to skip a spot. Items after the sixth one are simply ignored.
listToCardsOnTable :: [Maybe (Creature Core)] -> CardsOnTable
listToCardsOnTable maybeCreatures =
  impl (take (length spots) maybeCreatures) 0 Map.empty
  where
    spots :: [CardSpot] = [TopLeft ..]
    impl :: [Maybe (Creature Core)] -> Int -> CardsOnTable -> CardsOnTable
    impl [] idx acc = acc
    impl (fst : tail) idx acc =
      let nextAcc =
            case fst of
              Nothing -> acc
              Just creature -> Map.insert (spots !! idx) creature acc
       in impl tail (idx + 1) nextAcc

type CardsInHand = Set.Set (Card Core)

data PlayerPart
  = PlayerPart
      { visible :: CardsOnTable,
        invisible :: CardsInHand
      }

data PlayerSpot = PlayerBottom | PlayerTop
  deriving (Eq, Ord)

type Board = Map.Map PlayerSpot PlayerPart
