{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CardUi
  ( creatureAssetsIds,
    creatureID2FilePath,
  )
where

import Card
import Constants
import Data.List.Extra (lower)

-- | Filenames components: ["undead-archer", "human-spearman", ..]
creatureAssetsIds :: [(String, String)]
creatureAssetsIds =
  let teamStrings = map presentIDMember teams
      creaturesStrings = map presentIDMember creatures
   in [(t, c) | t <- teamStrings, c <- creaturesStrings]
  where
    teams = [Human ..]
    creatures = [Spearman ..]

creatureID2Filename :: CreatureID -> FilePath
creatureID2Filename CreatureID {creatureKind, team} =
  kgenPrefix
    ++ presentIDMember team
    ++ [assetGenSep]
    ++ presentIDMember creatureKind
    ++ dotPng

creatureID2FilePath :: CreatureID -> FilePath
creatureID2FilePath creatureID =
  assetsGenPath ++ "/" ++ creatureID2Filename creatureID

presentIDMember :: (Show a) => a -> String
presentIDMember = lower . show
