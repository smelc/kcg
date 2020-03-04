module Main where

import Card
import Data.ByteString.Lazy
import Json

main :: IO ()
main = do
  content <- Data.ByteString.Lazy.readFile dataFile
  print $ readJson content
  where
    dataFile = "data.json"
