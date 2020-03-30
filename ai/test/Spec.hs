{-# LANGUAGE OverloadedStrings #-}

import Constants
import qualified Data.ByteString.Lazy as ByteString
import Data.Either
import Json
import Test.Hspec

main :: IO ()
main = do
  dataByteString <- ByteString.readFile dataFile
  hspec $ do
    describe ("read " ++ dataFile) $ do
      it "reading should succeed" $ do
        parseJson dataByteString `shouldSatisfy` isRight
