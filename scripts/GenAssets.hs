{-# LANGUAGE ScopedTypeVariables #-}

-- To launch [ghcid](https://github.com/ndmitchell/ghcid)
-- in a terminal, do:
--   stack exec ghcid -- --command="ghci GenAssets.hs"

module GenAssets where

import System.Exit
import System.IO
import System.Process

fileToThrice :: [(String, String)]
fileToThrice =
  [ ("assets/16x16.png", cardgenResources),
    ("assets/24x24.png", cardgenResources)
  ]
  where
    cardgenResources = "cardgen/src/commonMain/resources"

gitRoot :: IO (ExitCode, String, String) -- value, stdout, stderr
gitRoot = do
  readCreateProcessWithExitCode createProcess ""
  where
    createProcess = shell "git rev-parse --show-toplevel"

needsCopy ::
  -- | git root
  FilePath ->
  -- | The file to check
  FilePath ->
  -- | Whether the file has a modification
  IO (Maybe Bool)
needsCopy gitRoot filepath = do
  (rc, procOut, procErr) <- readCreateProcessWithExitCode createProcess ""
  result <-
    case rc of
      ExitFailure _ -> do
        hPutStrLn stderr procErr
        return Nothing
      ExitSuccess ->
        return $ Just $ null procOut
  return result
  where
    createProcess = (shell $ "git diff " ++ filepath) {cwd = Just gitRoot}

mainRoot :: FilePath -> IO ExitCode
mainRoot gitRoot = do
  undefined

main :: IO ()
main = do
  (gitRootRC, gitRootStdout, gitRootStdErr) <- gitRoot
  rc <- case gitRootRC of
    ExitFailure code -> do
      print gitRootStdErr
      return $ ExitFailure code
    ExitSuccess ->
      mainRoot gitRootStdout
  exitWith rc
