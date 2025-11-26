module MyPhoto.AppToPNG
  ( runMyPhotoToPNG,
  )
where

import Data.Char (toLower)
import MyPhoto.Actions.UnRAW
import MyPhoto.Actions.UnTiff
import MyPhoto.Model
import System.Environment (getArgs, getProgName)
import System.FilePath (takeExtension)
import qualified System.IO as IO

printUsage :: IO ()
printUsage = do
  prog <- getProgName
  IO.hPutStrLn IO.stderr ("Usage: " ++ prog ++ " FILE [FILE...]")
  IO.hPutStrLn IO.stderr "Converts RAW->TIFF->PNG when applicable"

isExt :: [String] -> FilePath -> Bool
isExt exts fp = map toLower (takeExtension fp) `elem` exts

runMyPhotoToPNG :: IO ()
runMyPhotoToPNG = do
  args <- getArgs
  case args of
    ("-h" : _) -> printUsage
    ("--help" : _) -> printUsage
    [] -> printUsage
    files -> do
      let rawFiles = filter (isExt unrawExtensions) files
          tiffFiles = filter (isExt untiffExtensions) files
      tiffsFromRaw <-
        if null rawFiles
          then return []
          else unRAW (def {urVerbose = True}) rawFiles
      pngs <-
        if null tiffFiles && null tiffsFromRaw
          then return []
          else unTiff False (tiffsFromRaw ++ tiffFiles)
      mapM_ putStrLn (tiffsFromRaw ++ pngs)
