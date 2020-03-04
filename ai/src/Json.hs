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

neutralOptions :: Options
neutralOptions =
  defaultOptions
    { fieldLabelModifier = impl
    }
  where
    impl "neutralName" = "name"
    impl s = s

neutralKindOptions :: Options
neutralKindOptions =
  defaultOptions
    { constructorTagModifier = impl
    }
  where
    impl = lower

instance ToJSON Creature

instance FromJSON Creature where
  parseJSON = genericParseJSON creatureOptions

instance ToJSON Neutral

instance FromJSON Neutral where
  parseJSON = genericParseJSON neutralOptions

instance ToJSON NeutralKind

instance FromJSON NeutralKind where
  parseJSON = genericParseJSON neutralKindOptions

data AllData
  = AllData
      { creatures :: [Creature],
        neutral :: [Neutral]
      }
  deriving (Generic, Show)

instance ToJSON AllData

instance FromJSON AllData

readJson ::
  -- | The content of data.json
  ByteString ->
  Either String [Card]
readJson json = do
  allData :: AllData <- eitherDecode json
  let creatureCards :: [Card] = Prelude.map CreatureCard (creatures allData)
      neutralCards :: [Card] = Prelude.map NeutralCard (neutral allData)
      allCards :: [Card] = creatureCards ++ neutralCards
  return allCards

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
