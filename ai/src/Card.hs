{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Card
  ( Card (..),
    Creature (..),
    Forall,
    Item,
    ItemObject (..),
    Neutral,
    NeutralObject (..),
    Phase (..),
    Skill,
    Team (..),
  )
where

import Data.Kind (Constraint, Type)
import GHC.Generics

data Team = Human | Undead
  deriving (Generic, Show)

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

data Creature (p :: Phase)
  = Creature
      { team :: Team,
        creatureName :: String,
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
