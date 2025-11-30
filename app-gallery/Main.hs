module Main where

import CmdAddMeta (parseAddArgs, runAddMeta)
import CmdImport (runImport)
import Model (PhotoMeta)
import System.Environment (getArgs)
import System.Exit (die)

data Command
  = CmdAdd [FilePath] PhotoMeta
  | CmdImport [FilePath]

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left err -> die err
    Right (CmdImport dirs) -> mapM_ runImport dirs
    Right (CmdAdd files cliMeta) -> mapM_ (runAddMeta cliMeta) files

parseArgs :: [String] -> Either String Command
parseArgs ("import" : dirs)
  | null dirs = Left usage
  | otherwise = Right (CmdImport dirs)
parseArgs args = do
  case parseAddArgs args of
    Left err -> Left err
    Right (cliMeta, files) -> Right (CmdAdd files cliMeta)

usage :: String
usage =
  unlines
    [ "Usage:"
    , "  myphoto-gallery [--tag TAG] [--about FILE] FILE [FILE ...]  # create sidecar metadata for FILE"
    , "  myphoto-gallery import PATH/TO/DIR    # import files with sidecar metadata into CWD"
    ]
