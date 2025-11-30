module Main where

import CmdAddMeta (runAddMeta)
import CmdImport (runImport)
import qualified Data.Set as Set
import System.Environment (getArgs)
import System.Exit (die)

data Command
  = CmdAdd [FilePath] (Set.Set String) [FilePath]
  | CmdImport [FilePath]

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left err -> die err
    Right (CmdImport dirs) -> mapM_ runImport dirs
    Right (CmdAdd files tagSet abouts) -> mapM_ (runAddMeta tagSet abouts) files

parseArgs :: [String] -> Either String Command
parseArgs ("import" : dirs)
  | null dirs = Left usage
  | otherwise = Right (CmdImport dirs)
parseArgs args = do
  let (tagsList, aboutList, files) = gather args ([], [], [])
  if null files
    then Left usage
    else Right (CmdAdd (reverse files) (Set.fromList tagsList) (reverse aboutList))
  where
    gather [] acc = acc
    gather ("--tag" : t : xs) (ts, as, fs) = gather xs (t : ts, as, fs)
    gather ("--about" : p : xs) (ts, as, fs) = gather xs (ts, p : as, fs)
    gather (x : xs) (ts, as, fs) = gather xs (ts, as, x : fs)

usage :: String
usage =
  unlines
    [ "Usage:"
    , "  myphoto-gallery [--tag TAG] [--about FILE] FILE [FILE ...]  # create sidecar metadata for FILE"
    , "  myphoto-gallery import PATH/TO/DIR    # import files with sidecar metadata into CWD"
    ]
