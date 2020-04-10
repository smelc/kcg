{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Card
import Constants
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as ByteString
import Data.Either
import Data.Either.Combinators hiding (isLeft)
import Data.Maybe
import Json
import MainUi
import qualified Options.Applicative as Opt
import System.Exit

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
  let eitherUiData :: Either String [Card UI] = parseJson content
  when (isLeft eitherUiData) $ do
    putStrLn $ fromLeft' eitherUiData
    exitWith $ ExitFailure 1
  let uiData :: [Card UI] = fromRight' eitherUiData
  putStrLn $ "Interpreted " ++ dataFile
  print uiData
  case optUIMode of
    UIYes -> do
      Prelude.putStrLn "Opening UI"
      let creatureIDs = catMaybes $ map card2CreatureId uiData
      assets <- loadAssets creatureIDs
      mainUI assets
    UINo -> return ()
  putStrLn "Terminating"
