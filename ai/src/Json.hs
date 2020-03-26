{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

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

instance FromJSON Team where
  parseJSON = genericParseJSON toLowerConstructorOptions

toLowerConstructorOptions :: Options
toLowerConstructorOptions =
  defaultOptions
    { constructorTagModifier = lower
    }

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

instance ToJSON (Creature UI)

instance FromJSON (Creature UI) where
  parseJSON = genericParseJSON creatureOptions

instance ToJSON Neutral

instance FromJSON Neutral where
  parseJSON = genericParseJSON toLowerConstructorOptions

instance ToJSON NeutralObject

instance FromJSON NeutralObject where
  parseJSON = genericParseJSON neutralObjectOptions

instance ToJSON Item

instance FromJSON Item where
  parseJSON = genericParseJSON toLowerConstructorOptions

instance ToJSON ItemObject

instance FromJSON ItemObject where
  parseJSON = genericParseJSON itemObjectOptions

data AllData (p :: Phase)
  = AllData
      { creatures :: [Creature p],
        neutral :: [NeutralObject],
        items :: [ItemObject]
      }
  deriving (Generic)

deriving instance Forall Show p => Show (AllData p)

instance ToJSON (AllData UI)

instance FromJSON (AllData UI)

readJson ::
  -- | The content of data.json
  ByteString ->
  Either String [Card UI]
readJson json = do
  AllData creatures neutral items <- eitherDecode json
  let creatureCards = Prelude.map CreatureCard creatures
      neutralCards = Prelude.map (NeutralCard . Card.neutral) neutral
      itemCards = Prelude.map (ItemCard . Card.item) items
      allCards = creatureCards ++ neutralCards ++ itemCards
  return allCards
