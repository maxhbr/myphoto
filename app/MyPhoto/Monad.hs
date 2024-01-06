module MyPhoto.Monad where

import Control.Concurrent (getNumCapabilities)
import qualified Control.Monad.State.Lazy as MTL
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import MyPhoto.Actions.Align
import MyPhoto.Actions.Enfuse
import MyPhoto.Actions.Metadata
import MyPhoto.Actions.FileSystem
import MyPhoto.Actions.FocusStack
import MyPhoto.Actions.Montage
import MyPhoto.Actions.Outliers
import MyPhoto.Model
import MyPhoto.Video
import System.Console.GetOpt
import System.Environment (getArgs, getProgName, withArgs)
import qualified System.IO as IO
import qualified GHC.IO.Handle as IO


startOptions :: Options
startOptions =
  Options
    { optVerbose = False,
      optWorkdirStrategy = CreateNextToImgDir,
      optEveryNth = Nothing,
      optSortOnCreateDate = True,
      optRemoveOutliers = False,
      optBreaking = Nothing,
      optEnfuse = True,
      optFocusStack = True,
      optParameters = mempty
    }

data MyPhotoState
  = MyPhotoState
      { myPhotoStateOpts :: Options,
        myPhotoStateImgs :: Imgs,
        myPhotoStateOuts :: Imgs,
        myPhotoStateWd :: Maybe FilePath
      }
instance Show MyPhotoState where
  show s =
    "MyPhotoState {\n  myPhotoStateOpts = "
      ++ show (myPhotoStateOpts s)
      ++ ",\n  myPhotoStateImgs = "
      ++ show (length (myPhotoStateImgs s))
      ++ ",\n  myPhotoStateOuts = "
      ++ show (myPhotoStateOuts s)
      ++ ",\n  myPhotoStateWd = "
      ++ show (myPhotoStateWd s)
      ++ "\n}"
startMyPhotoState :: Imgs -> MyPhotoState
startMyPhotoState imgs =
  MyPhotoState
    { myPhotoStateOpts = startOptions,
      myPhotoStateImgs = imgs,
      myPhotoStateOuts = [],
      myPhotoStateWd = Nothing
    }
type MyPhotoM = MTL.StateT MyPhotoState IO
logDebugIO :: String -> IO ()
logDebugIO msg = IO.hPutStrLn IO.stderr ("DEBUG: " ++ msg)
logDebug :: String -> MyPhotoM ()
logDebug msg = do
  opts <- getOpts
  when (optVerbose opts) $ MTL.liftIO $ logDebugIO msg
logInfoIO :: String -> IO ()
logInfoIO msg = IO.hPutStrLn IO.stderr ("INFO: " ++ msg)
logInfo :: String -> MyPhotoM ()
logInfo msg = MTL.liftIO $ logInfoIO msg
logWarnIO :: String -> IO ()
logWarnIO msg = IO.hPutStrLn IO.stderr ("WARN: " ++ msg)
logWarn :: String -> MyPhotoM ()
logWarn msg = MTL.liftIO $ logWarnIO msg
logErrorIO :: String -> IO ()
logErrorIO msg = IO.hPutStrLn IO.stderr ("ERROR: " ++ msg)
logError :: String -> MyPhotoM ()
logError msg = MTL.liftIO $ logErrorIO msg
getOpts :: MyPhotoM Options
getOpts = MTL.gets myPhotoStateOpts
withOpts :: (Options -> Options) -> MyPhotoM ()
withOpts f = MTL.modify (\s -> s {myPhotoStateOpts = f (myPhotoStateOpts s)})
withOptsIO :: (Options -> IO Options) -> MyPhotoM ()
withOptsIO f = do
  opts <- getOpts
  opts' <- MTL.liftIO $ f opts
  withOpts (const opts')
getImgs :: MyPhotoM Imgs
getImgs = MTL.gets myPhotoStateImgs
putImgs :: Imgs -> MyPhotoM ()
putImgs imgs = MTL.modify (\s -> s {myPhotoStateImgs = imgs})
withImgs :: (Imgs -> Imgs) -> MyPhotoM ()
withImgs f = MTL.modify (\s -> s {myPhotoStateImgs = f (myPhotoStateImgs s)})
withImgsIO :: (Imgs -> IO Imgs) -> MyPhotoM ()
withImgsIO f = do
  imgs <- getImgs
  imgs' <- MTL.liftIO $ f imgs
  withImgs (const imgs')
getOuts :: MyPhotoM Imgs
getOuts = MTL.gets myPhotoStateOuts
addOut :: FilePath -> MyPhotoM ()
addOut out = MTL.modify (\s -> s {myPhotoStateOuts = myPhotoStateOuts s ++ [out]})
addOuts :: [FilePath] -> MyPhotoM ()
addOuts outs = MTL.modify (\s -> s {myPhotoStateOuts = myPhotoStateOuts s ++ outs})
withOutsIO :: (Imgs -> IO Imgs) -> MyPhotoM ()
withOutsIO f = do
  outs <- getOuts
  outs' <- MTL.liftIO $ f outs
  addOuts outs'
setWd :: FilePath -> MyPhotoM ()
setWd wd = MTL.modify (\s -> s {myPhotoStateWd = Just wd})


redirectLogToLogFile :: MyPhotoM a -> MyPhotoM a
redirectLogToLogFile action = do
  wd <- MTL.gets myPhotoStateWd
  case wd of
    Nothing -> action
    Just wd' -> do
      let logFile = wd' ++ "/myphoto.log"
      logInfo $ "Redirecting log to " ++ logFile
      state <- MTL.get
      (ret, endState) <- MTL.liftIO $ IO.withFile logFile IO.AppendMode $ \logFile -> do
        -- redirect stdout to log file without losing the actual output
        IO.hDuplicateTo logFile IO.stdout
        IO.hSetBuffering IO.stdout IO.LineBuffering
        IO.hDuplicateTo logFile IO.stderr
        IO.hSetBuffering IO.stderr IO.LineBuffering

        MTL.runStateT action state
      MTL.put endState
      return ret
