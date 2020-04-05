{-# LANGUAGE ScopedTypeVariables #-}

module CardUi where

import Card
import Data.List.Extra (lower)

-- | Filenames components: ["undead-archer", "human-spearman", ..]
creatureAssetsIds :: [(String, String)]
creatureAssetsIds =
  let teamStrings = map present teams
      creaturesStrings = map present creatures
   in [(t, c) | t <- teamStrings, c <- creaturesStrings]
  where
    teams = [Human ..]
    creatures = [Spearman ..]
    present :: (Show a) => a -> String
    present = lower . show
