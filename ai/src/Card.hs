{-# LANGUAGE DeriveGeneric #-}

module Card
  ( Card (..),
    Creature (..),
    Item,
    ItemObject (..),
    Neutral,
    NeutralObject (..),
    Skill,
    Team (..),
  )
where

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

data Creature
  = Creature
      { team :: Team,
        creatureName :: String,
        hp :: Int,
        attack :: Int,
        moral :: Maybe Int,
        victoryPoints :: Int,
        skills :: Maybe [Skill]
      }
  deriving (Generic, Show)

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
  deriving (Generic, Show)

newtype ItemObject
  = ItemObject
      {item :: Item}
  deriving (Generic, Show)

data Card
  = CreatureCard Creature
  | NeutralCard Neutral
  | ItemCard Item
  deriving (Show)
