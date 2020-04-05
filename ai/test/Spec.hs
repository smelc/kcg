{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Card
import CardUi
import Constants
import Control.Monad (forM_)
import qualified Data.ByteString.Lazy as ByteString (readFile)
import Data.Either (isRight)
import Data.List (isPrefixOf, isSuffixOf)
import Data.List.Split (wordsBy)
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
      creatures :: [CreatureKind] = [Spearman ..]
      assetsIds :: [(String, String)] = creatureAssetsIds
  hspec $ do
    describe ("read " ++ dataFile) $ do
      it "reading should succeed" $ do
        parseJson dataByteString `shouldSatisfy` isRight
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
