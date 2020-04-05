{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CardUi where

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

creatureID2AssetFilename :: CreatureID -> FilePath
creatureID2AssetFilename CreatureID {creatureKind, team} =
  presentIDMember creatureKind
    ++ [assetGenSep]
    ++ presentIDMember team
    ++ dotPng

presentIDMember :: (Show a) => a -> String
presentIDMember = lower . show
