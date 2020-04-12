{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Board
import Card
import CardUi
import Constants
import Control.Monad (forM_)
import qualified Data.ByteString.Lazy as ByteString (readFile)
import Data.Either (isRight)
import Data.Either.Extra (fromRight')
import Data.List (isPrefixOf, isSuffixOf)
import Data.List.Split (wordsBy)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Json
import System.Directory (listDirectory)
import Test.Hspec
import Text.Printf (printf)

genAssetsIdentifiers ::
  [FilePath] ->
  [(String, String)]
genAssetsIdentifiers assets =
  let -- Members of the form "kgen-\1+\2.png"
      assets' =
        filter
          ( \x ->
              isPrefixOf prefix x
                && isSuffixOf suffix x
                && sep `elem` x
          )
          assets
      -- '\1+\2' part
      assets'' :: [String] = map trim assets'
      assets''' :: [[String]] =
        filter
          (\x -> length x == 2)
          $ map (wordsBy (== sep)) assets''
   in map (\x -> (x !! 0, x !! 1)) assets'''
  where
    prefix = kgenPrefix
    suffix :: [Char] = ".png"
    sep :: Char = assetGenSep
    trim :: String -> String = take' (length suffix) . drop (length prefix)
    take' n l = take (length l - n) l

main :: IO ()
main = do
  dataByteString <- ByteString.readFile dataFile
  assets :: [FilePath] <- listDirectory assetsGenPath
  let assetsOnDisk :: [(String, String)] = genAssetsIdentifiers assets
      teams :: [Team] = [Human ..]
      spots :: [CardSpot] = [TopLeft ..]
      creatures :: [CreatureKind] = [Spearman ..]
      assetsIds :: [(String, String)] = creatureAssetsIds
      eitherCards = parseJson dataByteString
  hspec $ do
    describe ("read " ++ dataFile) $ do
      it "reading should succeed" $ do
        eitherCards `shouldSatisfy` isRight
    describe ("enum iterations are correct") $ do
      it "Team enumeration is correct" $ do
        fromEnum (head teams) `shouldBe` 0
      it "CreatureKind enumeration is correct" $ do
        fromEnum (head creatures) `shouldBe` 0
    describe "identifiers suffice"
      $ forM_ assetsOnDisk
      $ \assetOnDisk -> do
        let descr :: String =
              printf
                "%s%c%s has matching constructors"
                (fst assetOnDisk)
                assetGenSep
                (snd assetOnDisk)
        it descr $
          (assetOnDisk `elem` assetsIds) `shouldBe` True
    describe "broken identifier not found" $ do
      let badTeam :: String = "badTeam"
          badID = "foo"
      it (badTeam ++ [assetGenSep] ++ badID ++ " has no matching constructors") $
        ((badTeam, badID) `elem` assetsIds) `shouldBe` False
  let cards :: [Card UI] = fromRight' eitherCards
      creatures' :: [Creature Core] =
        mapMaybe
          (fmap creatureUI2CreatureCore . card2Creature)
          cards
  hspec $ do
    describe "board" $ do
      it "listToCardsOnTable yields a full board" $ do
        Map.size (listToCardsOnTable $ map Just creatures') `shouldBe` length spots
