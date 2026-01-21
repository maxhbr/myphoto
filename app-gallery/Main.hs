module Main where

import CmdAddMeta (parseAddArgs, runAddMeta)
import CmdImport (ImportOpts, parseImportArgs, runImportInit, runImportWithOpts, runUpdate)
import Model (PhotoMeta)
import System.Environment (getArgs)
import System.Exit (die)

data Command
  = CmdAdd [FilePath] PhotoMeta
  | CmdImport ImportOpts
  | CmdImportInit
  | CmdUpdate

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left err -> die err
    Right (CmdImport opts) -> runImportWithOpts opts
    Right CmdImportInit -> runImportInit
    Right (CmdAdd files cliMeta) -> mapM_ (runAddMeta cliMeta) files
    Right CmdUpdate -> runUpdate False

parseArgs :: [String] -> Either String Command
parseArgs ["update"] = Right CmdUpdate
parseArgs ["import", "--init"] = Right CmdImportInit
parseArgs ["import", "--init", _] = Left usage
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
      "  myphoto-gallery import [--dry-run] PATH/TO/DIR              # import files with sidecar metadata into CWD",
      "  myphoto-gallery import --init                               # create the root config in CWD",
      "  myphoto-gallery update                                      # update galleries based on imported metadata in CWD"
    ]
