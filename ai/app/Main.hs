module Main where

import Card
import qualified Data.ByteString.Lazy as ByteString
import Json
import MainUi

main :: IO ()
main = do
  Prelude.putStrLn $ "Reading " ++ dataFile
  content <- ByteString.readFile dataFile
  Prelude.putStrLn $ "Read " ++ dataFile ++ ". Interpreting its content."
  print $ readJson content
  Prelude.putStrLn $ "Interpreted " ++ dataFile ++ ". Over to the UI."
  mainUI
  where
    dataFile = "data.json"
