module MyPhoto.AppAlign
  ( runMyPhotoAlign,
  )
where

import MyPhoto.Actions.Align
import MyPhoto.Model
import System.Environment (getArgs, getProgName)
import qualified System.IO as IO

printUsage :: IO ()
printUsage = do
  prog <- getProgName
  IO.hPutStrLn IO.stderr ("Usage: " ++ prog ++ " FILE FILE [FILE [...]]")

runMyPhotoAlign :: IO ()
runMyPhotoAlign = do
  args <- getArgs
  case args of
    ("-h" : _) -> printUsage
    ("--help" : _) -> printUsage
    [] -> printUsage
    [_] -> return ()
    files -> do
      let opts = def {alignOptVerbose = True, alignOptUntiff = True}
      aligned <- align opts "." files
      mapM_ putStrLn aligned
