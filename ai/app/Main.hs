{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Card
import Control.Applicative
import qualified Data.ByteString.Lazy as ByteString
import Json
import MainUi
import qualified Options.Applicative as Opt

data UIMode = UIYes | UINo

data Options
  = Options
      { optUIMode :: UIMode
      }

optionsParser :: Opt.Parser Options
optionsParser =
  Options
    <$> Opt.flag
      UINo
      UIYes
      ( Opt.long "ui"
          <> Opt.help "runs the UI"
      )

optionsParserInfo :: Opt.ParserInfo Options
optionsParserInfo = Opt.info (optionsParser <**> Opt.helper) Opt.fullDesc

main :: IO ()
main = do
  Options {optUIMode} <- Opt.execParser optionsParserInfo
  putStrLn $ "Reading " ++ dataFile
  content <- ByteString.readFile dataFile
  putStrLn $ "Read " ++ dataFile ++ ". Interpreting its content."
  print $ readJson content
  putStrLn $ "Interpreted " ++ dataFile ++ "."
  case optUIMode of
    UIYes -> do
      Prelude.putStrLn "Opening UI"
      mainUI
    UINo -> return ()
  putStrLn "Terminating"
  where
    dataFile = "data.json"
