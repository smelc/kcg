{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
  ( someFunc,
  )
where

import Data.Aeson
import Data.ByteString.Lazy
import GHC.Generics

data Team = Human | Undead
  deriving (Generic, Show)

instance ToJSON Team

instance FromJSON Team

data Skill
  = HitFromBack
  | Leader
  | Ranged
  | Unique
  deriving (Generic, Show)

instance ToJSON Skill

instance FromJSON Skill

creatureOptions :: Options
creatureOptions =
  defaultOptions
    { fieldLabelModifier = impl
    }
  where
    impl "creatureName" = "name"
    impl "victoryPoints" = "victory_points"
    impl s = s

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
  = Neutral
      {neutralName :: String}
  deriving (Generic, Show)

neutralOptions :: Options
neutralOptions =
  defaultOptions
    { fieldLabelModifier = impl
    }
  where
    impl "neutralName" = "name"
    impl s = s

instance ToJSON Creature

instance FromJSON Creature where
  parseJSON = genericParseJSON creatureOptions

instance ToJSON Neutral

instance FromJSON Neutral where
  parseJSON = genericParseJSON neutralOptions

data AllData
  = AllData
      { creatures :: [Creature],
        neutral :: Maybe [Neutral]
      }
  deriving (Generic, Show)

instance ToJSON AllData

instance FromJSON AllData

someFunc :: IO ()
someFunc = do
  print (encode creature)
  print creatureJSON
  print decoding
  print decoding2
  print allDecoding
  where
    creature :: Creature = Creature Human "archer" 1 1 (Just 2) 2 Nothing
    creatureJSON = "{ \"team\":\"Human\", \"name\":\"spearman\",  \"hp\":2, \"attack\":1, \"victory_points\":2, \"skills\":[\"HitFromBack\"]}"
    creature2JSON = "{ \"team\":\"Human\", \"foobar\":\"ignore\", \"name\":\"spearman\",  \"hp\":2, \"attack\":1, \"victory_points\":2}"
    decoding :: Either String Creature = eitherDecode creatureJSON
    decoding2 :: Either String Creature = eitherDecode creature2JSON
    prefix = "{ \"creatures\": ["
    suffix = "] }"
    allDataJSON :: ByteString = append prefix $ append creatureJSON suffix
    allDecoding :: Either String AllData = eitherDecode allDataJSON
