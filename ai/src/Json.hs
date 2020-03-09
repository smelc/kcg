{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Json
  ( readJson,
  )
where

import Card
import Data.Aeson
import Data.ByteString.Lazy
import Data.List.Extra
import GHC.Generics

instance ToJSON Team

instance FromJSON Team

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

neutralObjectOptions :: Options
neutralObjectOptions =
  defaultOptions
    { fieldLabelModifier = impl
    }
  where
    impl "neutral" = "name"
    impl s = s

itemObjectOptions :: Options
itemObjectOptions =
  defaultOptions
    { fieldLabelModifier = impl
    }
  where
    impl "item" = "name"
    impl s = s

instance ToJSON Creature

instance FromJSON Creature where
  parseJSON = genericParseJSON creatureOptions

instance ToJSON Neutral

instance FromJSON Neutral

instance ToJSON NeutralObject

instance FromJSON NeutralObject where
  parseJSON = genericParseJSON neutralObjectOptions

instance ToJSON Item

instance FromJSON Item

instance ToJSON ItemObject

instance FromJSON ItemObject where
  parseJSON = genericParseJSON itemObjectOptions

data AllData
  = AllData
      { creatures :: [Creature],
        neutral :: [NeutralObject],
        items :: [ItemObject]
      }
  deriving (Generic, Show)

instance ToJSON AllData

instance FromJSON AllData

readJson ::
  -- | The content of data.json
  ByteString ->
  Either String [Card]
readJson json = do
  AllData creatures neutral items <- eitherDecode json
  let creatureCards :: [Card] = Prelude.map CreatureCard creatures
      neutralCards :: [Card] = Prelude.map (NeutralCard . Card.neutral) neutral
      itemCards :: [Card] = Prelude.map (ItemCard . Card.item) items
      allCards :: [Card] = creatureCards ++ neutralCards ++ itemCards
  return allCards
