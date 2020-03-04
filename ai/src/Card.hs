{-# LANGUAGE DeriveGeneric #-}

module Card
  ( Card (..),
    Creature (..),
    Neutral,
    NeutralKind,
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

data NeutralKind
  = Health
  | Life
  deriving (Generic, Show)

data Neutral
  = Neutral
      {neutralName :: NeutralKind}
  deriving (Generic, Show)

data Card
  = CreatureCard Creature
  | NeutralCard Neutral
  deriving (Show)
