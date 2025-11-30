module Main where

import CmdAddMeta (parseAddArgs, runAddMeta)
import CmdImport (ImportOpts, parseImportArgs, runImportWithOpts)
import Model (PhotoMeta)
import System.Environment (getArgs)
import System.Exit (die)

data Command
  = CmdAdd [FilePath] PhotoMeta
  | CmdImport ImportOpts

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left err -> die err
    Right (CmdImport opts) -> runImportWithOpts opts
    Right (CmdAdd files cliMeta) -> mapM_ (runAddMeta cliMeta) files

parseArgs :: [String] -> Either String Command
parseArgs ("import" : dirs)
  | null dirs = Left usage
  | otherwise =
      case parseImportArgs dirs of
        Left err -> Left err
        Right opts -> Right (CmdImport opts)
parseArgs args = do
  case parseAddArgs args of
    Left err -> Left err
    Right (cliMeta, files) -> Right (CmdAdd files cliMeta)

usage :: String
usage =
  unlines
    [ "Usage:",
      "  myphoto-gallery [--tag TAG] [--about FILE] FILE [FILE ...]  # create sidecar metadata for FILE",
      "  myphoto-gallery import [--dry-run] PATH/TO/DIR    # import files with sidecar metadata into CWD"
    ]
