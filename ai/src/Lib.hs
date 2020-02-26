{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( someFunc
    ) where

import Data.Aeson
import GHC.Generics

data Team = Human | Undead
  deriving (Generic, Show)

instance ToJSON Team
instance FromJSON Team

data Skill = 
    HitFromBack
  | Ranged
  deriving (Generic, Show)

instance ToJSON Skill
instance FromJSON Skill

data Creature = Creature
  { team::Team,
    name::String,
    hp::Int,
    attack::Int,
    victoryPoints::Int,
    skills::[Skill] }
  deriving (Generic, Show)

instance ToJSON Creature
instance FromJSON Creature

someFunc :: IO ()
someFunc = do
  print (encode creature)
  print creatureJSON
  print decoding
  where creature :: Creature = Creature Human "archer" 1 1 2 []
        creatureJSON = "{ \"team\":\"Human\", \"name\":\"spearman\",  \"hp\":2, \"attack\":1, \"victoryPoints\":2, \"skills\":[\"HitFromBack\"]}"
        decoding :: Either String Creature = eitherDecode creatureJSON

