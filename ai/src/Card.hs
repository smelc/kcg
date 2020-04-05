{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Card where

import Data.Kind (Constraint, Type)
import GHC.Generics

data Team = Human | Undead
  deriving (Enum, Eq, Generic, Show, Ord)

data Skill
  = HitFromBack
  | Flammable
  | Leader
  | Ranged
  | Unique
  deriving (Generic, Show)

data Phase = UI | Core

type family CoordType (p :: Phase) where
  CoordType UI = Int
  CoordType Core = ()

type Forall (c :: Type -> Constraint) (p :: Phase) =
  ( c (CoordType p)
  )

data CreatureKind
  = Spearman
  | Swordsman
  | Archer
  | General
  | Skeleton
  | Vampire
  | Mummy
  deriving (Enum, Eq, Generic, Ord, Show)

data CreatureID = CreatureID {creatureKind :: CreatureKind, team :: Team}
  deriving (Eq, Generic, Ord, Show)

data Creature (p :: Phase)
  = Creature
      { id :: CreatureID,
        hp :: Int,
        attack :: Int,
        moral :: Maybe Int,
        victoryPoints :: Int,
        skills :: Maybe [Skill],
        x :: CoordType p,
        y :: CoordType p
      }
  deriving (Generic)

deriving instance Forall Show p => Show (Creature p)

data Neutral
  = Health
  | Life
  deriving (Generic, Show)

newtype NeutralObject
  = NeutralObject
      {neutral :: Neutral}
  deriving (Generic, Show)

data Item
  = Crown
  | FooBar
  deriving (Generic, Show)

newtype ItemObject
  = ItemObject
      {item :: Item}
  deriving (Generic, Show)

data Card (p :: Phase)
  = CreatureCard (Creature p)
  | NeutralCard Neutral
  | ItemCard Item

deriving instance Forall Show p => Show (Card p)
