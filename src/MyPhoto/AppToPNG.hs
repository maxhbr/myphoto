module MyPhoto.AppToPNG
  ( runMyPhotoToPNG,
  )
where

import MyPhoto.Actions.UnRAW
import MyPhoto.Actions.UnTiff
import MyPhoto.Model
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import qualified System.IO as IO

printUsage :: IO ()
printUsage = do
  prog <- getProgName
  IO.hPutStrLn IO.stderr ("Usage: " ++ prog ++ " [--clean] [--help] FILE [FILE...]")
  IO.hPutStrLn IO.stderr "Converts RAW->TIFF->PNG when applicable"
  IO.hPutStrLn IO.stderr "Flags: --clean removes source RAWs and intermediate TIFFs after conversion"

isExt :: [String] -> FilePath -> Bool
isExt exts fp = map toLower (takeExtension fp) `elem` exts

newtype ToPNGOptions = ToPNGOptions
  { optCleanInputs :: Bool
  }

defaultOptions :: ToPNGOptions
defaultOptions =
  ToPNGOptions
    { optCleanInputs = False
    }

data ParseResult
  = ShowHelp
  | ParseError String
  | ParsedArgs ToPNGOptions [FilePath]

parseArgs :: [String] -> ParseResult
parseArgs = go defaultOptions []
  where
    go _ _ ("-h" : _) = ShowHelp
    go _ _ ("--help" : _) = ShowHelp
    go opts files ("--clean" : rest) = go opts {optCleanInputs = True} files rest
    go _ _ (x@('-' : _) : _) = ParseError ("Unknown flag: " ++ x)
    go opts files (f : rest) = go opts (f : files) rest
    go opts files [] = ParsedArgs opts (reverse files)

runMyPhotoToPNG :: IO ()
runMyPhotoToPNG = do
  args <- getArgs
  case parseArgs args of
    ShowHelp -> printUsage >> exitSuccess
    ParseError msg -> IO.hPutStrLn IO.stderr msg >> printUsage >> exitFailure
    ParsedArgs _ [] -> printUsage >> exitFailure
    ParsedArgs opts files -> do
      let rawFiles = filter (isExt unrawExtensions) files
          tiffFiles = filter (isExt untiffExtensions) files
      tiffsFromRaw <-
        if null rawFiles
          then return []
          else unRAW (def {urVerbose = True, urCleanup = optCleanInputs opts}) rawFiles
      pngs <-
        if null tiffFiles && null tiffsFromRaw
          then return []
          else unTiff (optCleanInputs opts) (tiffsFromRaw ++ tiffFiles)
      mapM_ putStrLn (tiffsFromRaw ++ pngs)
