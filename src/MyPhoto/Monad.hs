module MyPhoto.Monad where

import qualified Control.Monad.State.Lazy as MTL
import Data.List (nub)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (getCurrentTimeZone, utcToLocalTime)
import MyPhoto.Model
import qualified System.IO as IO

data MyPhotoState = MyPhotoState
  { myPhotoStateOpts :: Options,
    myPhotoStateImgs :: Imgs,
    myPhotoStateOuts :: Imgs,
    myPhotoStateWd :: Maybe FilePath,
    myPhotoStartTime :: UTCTime,
    myPhotoLastLogTime :: UTCTime,
    myPhotoLogHandle :: Maybe IO.Handle
  }

instance Show MyPhotoState where
  show s =
    "MyPhotoState {\n  myPhotoStateOpts = "
      ++ show (myPhotoStateOpts s)
      ++ ",\n  myPhotoStateImgs = #"
      ++ show (length (myPhotoStateImgs s))
      ++ ",\n  myPhotoStateOuts = "
      ++ show (myPhotoStateOuts s)
      ++ ",\n  myPhotoStateWd = "
      ++ show (myPhotoStateWd s)
      ++ ",\n  myPhotoStartTime = "
      ++ show (myPhotoStartTime s)
      ++ ",\n  myPhotoLogHandle = "
      ++ maybe "Nothing" (const "Just <handle>") (myPhotoLogHandle s)
      ++ "\n}"

startMyPhotoState :: Options -> Imgs -> IO MyPhotoState
startMyPhotoState startOptions' imgs = do
  startTime <- getCurrentTime
  return $
    MyPhotoState
      { myPhotoStateOpts = startOptions',
        myPhotoStateImgs = imgs,
        myPhotoStateOuts = [],
        myPhotoStateWd = Nothing,
        myPhotoStartTime = startTime,
        myPhotoLastLogTime = startTime,
        myPhotoLogHandle = Nothing
      }

type MyPhotoM = MTL.StateT MyPhotoState IO

getLogHandle :: MyPhotoM (Maybe IO.Handle)
getLogHandle = MTL.gets myPhotoLogHandle

setLogHandle :: Maybe IO.Handle -> MyPhotoM ()
setLogHandle h = MTL.modify (\s -> s {myPhotoLogHandle = h})

logToHandle :: IO.Handle -> String -> IO ()
logToHandle h msg = do
  IO.hPutStrLn h msg
  IO.hFlush h

logDebug :: String -> MyPhotoM ()
logDebug msg = do
  opts <- getOpts
  when (optVerbose opts) $ do
    MTL.liftIO $ logDebugIO msg
    mh <- getLogHandle
    case mh of
      Just h -> MTL.liftIO $ logToHandle h ("DEBUG: " ++ msg)
      Nothing -> return ()

logInfo :: String -> MyPhotoM ()
logInfo msg = do
  MTL.liftIO $ logInfoIO msg
  mh <- getLogHandle
  case mh of
    Just h -> MTL.liftIO $ logToHandle h ("INFO: " ++ msg)
    Nothing -> return ()

logWarn :: String -> MyPhotoM ()
logWarn msg = do
  MTL.liftIO $ logWarnIO msg
  mh <- getLogHandle
  case mh of
    Just h -> MTL.liftIO $ logToHandle h ("WARN: " ++ msg)
    Nothing -> return ()

logError :: String -> MyPhotoM ()
logError msg = do
  MTL.liftIO $ logErrorIO msg
  mh <- getLogHandle
  case mh of
    Just h -> MTL.liftIO $ logToHandle h ("ERROR: " ++ msg)
    Nothing -> return ()

getOpts :: MyPhotoM Options
getOpts = MTL.gets myPhotoStateOpts

withOpts :: (Options -> Options) -> MyPhotoM ()
withOpts f = MTL.modify (\s -> s {myPhotoStateOpts = f (myPhotoStateOpts s)})

withOptsIO :: (Options -> IO Options) -> MyPhotoM ()
withOptsIO f = do
  opts <- getOpts
  opts' <- MTL.liftIO $ f opts
  withOpts (const opts')

guardWithOpts :: (Options -> Bool) -> MyPhotoM () -> MyPhotoM ()
guardWithOpts f action = do
  opts <- getOpts
  when (f opts) action

guardByExtensions :: [String] -> MyPhotoM () -> MyPhotoM ()
guardByExtensions exts action = do
  imgs <- getImgs
  let exts' = map (map toLower) exts
  let predicate = (\img -> takeExtension (map toLower img) `elem` exts')
  let allMatchPred = all predicate imgs
  let anyMatchPred = any predicate imgs
  case (allMatchPred, anyMatchPred) of
    (True, _) -> action
    (False, True) -> fail $ "Some, but not all, images have extensions " ++ show exts
    (False, False) -> return ()

getImgs :: MyPhotoM Imgs
getImgs = MTL.gets myPhotoStateImgs

putImgs :: Imgs -> MyPhotoM ()
putImgs imgs = do
  logDebug $ "putImgs: " ++ show (length imgs)
  MTL.modify (\s -> s {myPhotoStateImgs = imgs})

withImgs :: (Imgs -> Imgs) -> MyPhotoM ()
withImgs f = MTL.modify (\s -> s {myPhotoStateImgs = f (myPhotoStateImgs s)})

withImgsIO :: (Imgs -> IO Imgs) -> MyPhotoM ()
withImgsIO f = do
  imgs <- getImgs
  imgs' <- MTL.liftIO $ f imgs
  putImgs imgs'

getOuts :: MyPhotoM Imgs
getOuts = MTL.gets myPhotoStateOuts

addOut :: FilePath -> MyPhotoM ()
addOut out = do
  outExists <- MTL.liftIO $ doesFileExist out
  if not outExists
    then logError $ "addOut: file does not exist: " ++ out
    else MTL.modify (\s -> s {myPhotoStateOuts = nub (myPhotoStateOuts s ++ [out])})

addOuts :: [FilePath] -> MyPhotoM ()
addOuts outs = mapM_ addOut outs

replaceOuts :: Imgs -> MyPhotoM ()
replaceOuts outs = MTL.modify (\s -> s {myPhotoStateOuts = outs})

withOutsIO :: (Imgs -> IO Imgs) -> MyPhotoM ()
withOutsIO f = do
  outs <- getOuts
  outs' <- MTL.liftIO $ f outs
  addOuts outs'

withOutsReplaceIO :: (Imgs -> IO Imgs) -> MyPhotoM ()
withOutsReplaceIO f = do
  outs <- getOuts
  outs' <- MTL.liftIO $ f outs
  replaceOuts outs'

setWd :: FilePath -> MyPhotoM ()
setWd wd = MTL.modify (\s -> s {myPhotoStateWd = Just wd})

logTimeSinceStart :: String -> MyPhotoM ()
logTimeSinceStart msg = do
  startTime <- MTL.gets myPhotoStartTime
  lastLogTime <- MTL.gets myPhotoLastLogTime
  currentTime <- MTL.liftIO getCurrentTime
  let diff = diffUTCTime currentTime startTime
  let diffSinceLastLog = diffUTCTime currentTime lastLogTime
  let completeMsg = msg ++ " (elapsed time: " ++ show diff ++ ", elapsed since last log: " ++ show diffSinceLastLog ++ ")"
  logInfo completeMsg
  MTL.modify (\s -> s {myPhotoLastLogTime = currentTime})

step :: String -> MyPhotoM a -> MyPhotoM a
step stepName action = do
  let border = replicate 79 '─'
      logBoxStart = logInfo $ "┌" ++ border
      logInBox msg = logInfo $ "│ " ++ msg
      logBoxEnd = logInfo $ "└" ++ border
  logBoxStart
  currentTimeBefore <- MTL.liftIO getCurrentTime
  tz <- MTL.liftIO getCurrentTimeZone
  let localTime = utcToLocalTime tz currentTimeBefore
      timeStr = formatTime defaultTimeLocale "%H:%M" localTime
  logInBox ("Starting step: " ++ stepName ++ " (at: " ++ timeStr ++ ")")
  actionResult <- action
  currentTimeAfter <- MTL.liftIO getCurrentTime
  logInBox ("Finished step: " ++ stepName ++ " (elapsed time: " ++ show (diffUTCTime currentTimeAfter currentTimeBefore) ++ ")")
  logBoxEnd
  return actionResult

openLogFileAt :: FilePath -> MyPhotoM ()
openLogFileAt logFile = do
  existing <- getLogHandle
  case existing of
    Just _ -> return ()
    Nothing -> do
      opts <- getOpts
      when (optRedirectLog opts) $ do
        MTL.liftIO $ createDirectoryIfMissing True (takeDirectory logFile)
        h <- MTL.liftIO $ IO.openFile logFile IO.AppendMode
        MTL.liftIO $ IO.hSetBuffering h IO.LineBuffering
        setLogHandle (Just h)
        logInfo $ "Logging to " ++ logFile

closeLogFile :: MyPhotoM ()
closeLogFile = do
  mh <- getLogHandle
  case mh of
    Just h -> do
      MTL.liftIO $ IO.hClose h
      setLogHandle Nothing
    Nothing -> return ()
