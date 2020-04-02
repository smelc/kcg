{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Card
import Constants
import Control.Monad (forM_)
import qualified Data.ByteString.Lazy as ByteString (readFile)
import Data.Either (isRight)
import Data.List (isPrefixOf, isSuffixOf)
import Data.List.Split (wordsBy)
import Json
import System.Directory (listDirectory)
import Test.Hspec

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
    sep :: Char = '+'
    trim :: String -> String = take (length suffix) . drop (length prefix)

main :: IO ()
main = do
  dataByteString <- ByteString.readFile dataFile
  assets :: [FilePath] <- listDirectory assetsGenPath
  let genAssets :: [(String, String)] = genAssetsIdentifiers assets
      teams :: [Team] = [Human ..]
      creatures :: [CreatureKind] = [Spearman ..]
  hspec $ do
    describe ("read " ++ dataFile) $ do
      it "reading should succeed" $ do
        parseJson dataByteString `shouldSatisfy` isRight
    describe ("enum iterations are correct") $ do
      it "Team enumeration is correct" $ do
        fromEnum (head teams) `shouldBe` 0
      it "Creatures enumeration is correct" $ do
        fromEnum (head creatures) `shouldBe` 0
-- forM_ genAssets $ \genAsset -> do
-- describe "identifiers are complete" $ do
--   it "kgen-\\1-\\2 files match identifiers"
--     $ checkGenAssetsMembers assets
