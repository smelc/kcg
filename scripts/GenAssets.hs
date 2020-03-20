{-# LANGUAGE ScopedTypeVariables #-}

-- To launch [ghcid](https://github.com/ndmitchell/ghcid)
-- in a terminal, do:
--   stack exec ghcid -- --command="ghci GenAssets.hs"
-- To execute:
--   stack exec runghc -- GenAssets.hs
-- To disable coc in vim if not working (do it right after opening!):
--   :CocDisable

module GenAssets where

import Data.Foldable
import Data.List.Extra
import Data.Maybe
import Debug.Trace
import System.Exit
import System.FilePath
import System.IO
import System.Process

-- | Input file, destination folder, scaling ratio
filesToScale :: [(String, String, Int)]
filesToScale =
  [ ("assets/16x16.png", cardgenResources, 2),
    ("assets/24x24.png", cardgenResources, 3)
  ]
  where
    cardgenResources = "cardgen/src/commonMain/resources"

gitRoot :: IO (Maybe String) -- result
gitRoot = do
  (rc, outmsg, errmsg) <- readCreateProcessWithExitCode createProcess ""
  case rc of
    ExitSuccess -> return $ Just $ trim outmsg
    ExitFailure _ -> do
      hPutStrLn stderr errmsg
      return $ Nothing
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

-- Returns whether scaling went well
scaleFile :: FilePath -> FilePath -> FilePath -> Int -> IO Bool
scaleFile gitRoot input destDir scaling = do
  print filename
  (rc, outmsg, errmsg) <- readCreateProcessWithExitCode createProcess ""
  case rc of
    ExitSuccess -> return True
    ExitFailure _ -> do
      hPutStrLn stderr errmsg
      return False
  where
    -- If 'input' is 'assets/16x16.png', this is '16x16.png'
    filename = last $ splitOn [pathSeparator] input
    extLessFilename = head $ splitOn "." filename -- "16x16"
    extension = last $ splitOn "." filename -- "png"
          -- "16x16_x2.png"
    filename' = extLessFilename ++ "_" ++ show scaling ++ "." ++ extension
    destFile = destDir ++ [pathSeparator] ++ filename'
    cmd =
      "convert " ++ input
        ++ " -scale "
        ++ show (scaling * 100)
        ++ "% "
        ++ destFile
    createProcess = (shell $ trace cmd cmd) {cwd = Just gitRoot}

-- The main that executes with the root git directory known
mainRoot :: FilePath -> IO ExitCode
mainRoot gitRoot = do
  print gitRoot
  filesToDiff :: [(FilePath, Maybe Bool)] <-
    mapM (\(f, _, _) -> needsCopy' f) filesToScale
  if any isNothing (map snd filesToDiff)
    then do
      let log f = hPutStrLn stderr $ "Cannot find out if " ++ f ++ " has a diff"
          wrongs :: [(FilePath, Maybe Bool)] = (filter (\(_, mb) -> isNothing mb) filesToDiff)
          wrongs' :: [FilePath] = map fst wrongs
      traverse_ (\f -> log f) wrongs'
      return $ ExitFailure 1
    else do
      mapM scaleFile' filesToScale
      undefined
  where
    scaleFile' (t1, t2, t3) = scaleFile gitRoot t1 t2 t3
    needsCopy' :: FilePath -> IO (FilePath, Maybe Bool)
    needsCopy' f = do
      maybeBool <- needsCopy gitRoot f
      return $ (f, maybeBool)

main :: IO ()
main = do
  maybeGitRoot <- gitRoot
  rc <-
    case maybeGitRoot of
      Nothing -> return $ ExitFailure 1
      Just gitRootPath -> mainRoot gitRootPath
  exitWith rc
