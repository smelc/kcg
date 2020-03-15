{-# LANGUAGE ScopedTypeVariables #-}

-- To launch [ghcid](https://github.com/ndmitchell/ghcid)
-- in a terminal, do:
--   stack exec ghcid -- --command="ghci GenAssets.hs"

module GenAssets where

import System.Exit
import System.Process

fileToThrice :: [(String, String)]
fileToThrice =
  [ ("assets/16x16.png", cardgenResources),
    ("assets/24x24.png", cardgenResources)
  ]
  where
    cardgenResources = "cardgen/src/commonMain/resources"

gitRoot :: IO (Either String FilePath)
gitRoot = do
  (exitCode, stdout, _) <- readCreateProcessWithExitCode createProcess ""
  return $
    case exitCode of
      ExitSuccess -> Right stdout
      _ -> Left "git root cannot be obtained"
  where
    createProcess = shell "git rev-parse --show-toplevel"

mainRoot :: FilePath -> IO ExitCode
mainRoot gitRoot = undefined

main :: IO ()
main = do
  maybeRoot :: Either String FilePath <- gitRoot
  case maybeRoot of
    Left error -> do
      print error
    Right root -> do
      mainRoot root
  undefined
