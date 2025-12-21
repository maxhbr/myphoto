{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module MyPhoto.AppWatch where

import Control.Concurrent as Thread
import Control.Exception (SomeException, catch)
import Control.Monad ((>=>))
import qualified Control.Monad.State.Lazy as MTL
import Data.List (isPrefixOf)
import Data.Time.Clock (UTCTime, diffUTCTime)
import Data.Time.Clock.POSIX (getCurrentTime, posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import MyPhoto.Actions.FileSystem (copy)
import MyPhoto.Actions.Metadata
import MyPhoto.Actions.UnRAW (unrawExtensions)
import MyPhoto.AppStack
import MyPhoto.Impl
import MyPhoto.Model
import MyPhoto.Monad
import System.Console.GetOpt
import System.Directory.Recursive (getFilesRecursive)
import System.Environment (getArgs, getProgName)
import qualified System.IO as IO
import System.Process (readProcessWithExitCode)

data WatchOptions = WatchOptions
  { optWatchVerbose :: Bool,
    optWatchStackOpts :: Options,
    optWatchOnce :: Bool,
    optWatchOnlyImport :: Bool,
    optUseRaw :: Bool,
    optClusterDistance :: Int,
    optOffset :: Int,
    optIndir :: FilePath,
    optOutdir :: FilePath
  }
  deriving (Show, Eq)

printHelp :: IO ()
printHelp = do
  prg <- getProgName
  IO.hPutStr IO.stderr (usageInfo prg watchOptions)
  IO.hPutStrLn IO.stderr ""
  IO.hPutStrLn IO.stderr "Arguments: [INDIR] [OUTDIR]"
  IO.hPutStrLn IO.stderr "Stack Arguments can be passed after \"--\""
  IO.hPutStrLn IO.stderr ""
  IO.hPutStrLn IO.stderr "Examples:"
  IO.hPutStrLn IO.stderr "  myphoto-watch /path/to/input"
  IO.hPutStrLn IO.stderr "  myphoto-watch --once /path/to/input /path/to/output"
  IO.hPutStrLn IO.stderr "  myphoto-watch --raw --offset 12 /path/to/input"

watchOptions :: [OptDescr (WatchOptions -> IO WatchOptions)]
watchOptions =
  [ Option
      "v"
      ["verbose"]
      ( NoArg
          (\opt -> return opt {optWatchVerbose = True})
      )
      "Enable verbose messages",
    Option
      ""
      ["clean"]
      ( NoArg
          (\opt -> return opt {optWatchStackOpts = (optWatchStackOpts opt) {optClean = RemoveWorkdirRecursively}})
      )
      "Enable cleaning of the output directory",
    Option
      ""
      ["once"]
      ( NoArg
          (\opt -> return opt {optWatchOnce = True})
      )
      "Run once instead of continuous watching",
    Option
      ""
      ["only-import"]
      ( NoArg
          (\opt -> return opt {optWatchOnlyImport = True})
      )
      "Only import images, no processing",
    Option
      ""
      ["raw"]
      ( NoArg
          ( \opt ->
              return
                opt
                  { optUseRaw = True,
                    optWatchStackOpts = (optWatchStackOpts opt) {optEnfuse = False}
                  }
          )
      )
      "Use RAW files instead of JPG",
    Option
      ""
      ["cluster-distance"]
      ( ReqArg
          (\arg opt -> return opt {optClusterDistance = read arg})
          "SECONDS"
      )
      "Max distance in seconds between shots in a stack (default: 6)",
    Option
      ""
      ["offset"]
      ( ReqArg
          (\arg opt -> return opt {optOffset = (read arg :: Int) * 60 * 60})
          "HOURS"
      )
      "Time offset in hours (default: 12)",
    Option
      "h"
      ["help"]
      ( NoArg
          ( \_ -> do
              printHelp
              exitWith ExitSuccess
          )
      )
      "Show help"
  ]

jpgExtensions = [".jpg", ".JPG"]

loopIntervalInSeconds = 40

data WatchForStacksFile = WatchForStacksFile
  { wfsfPath :: Img,
    -- , wfsfSize :: Int
    -- , wfsfHash :: String
    wfsfExifTimeSeconds :: Int
  }
  deriving (Show, Eq)

data WatchForStacksState = WatchForStacksState
  { wfsOpts :: WatchOptions,
    wfsExtensions :: [String],
    wfsMinimalTime :: Int,
    wfsOldInFiles :: [FilePath],
    wfsFailedInFiles :: [FilePath],
    wfsInFileClusters :: [[WatchForStacksFile]],
    wfsFinishedClusters :: [[WatchForStacksFile]]
  }
  deriving (Show, Eq)

type WatchForStacksM = MTL.StateT WatchForStacksState IO

knowsFile :: FilePath -> WatchForStacksM Bool
knowsFile img = do
  WatchForStacksState
    { wfsFailedInFiles = failedFiles,
      wfsInFileClusters = inFiles,
      wfsOldInFiles = oldFiles,
      wfsFinishedClusters = finishedClusters
    } <-
    MTL.get
  let filePathsInInFiles = map wfsfPath (concat inFiles)
  let filePathsInFinishedClusters = map wfsfPath (concat finishedClusters)
  return $ img `elem` (filePathsInInFiles ++ filePathsInFinishedClusters ++ failedFiles ++ oldFiles)

analyzeFile :: FilePath -> IO WatchForStacksFile
analyzeFile p = do
  Metadata {_createDate = exifTimeSeconds} <- getMetadataFromImg False p
  let isoDate = posixSecondsToUTCTime (fromIntegral exifTimeSeconds)
  return $ WatchForStacksFile p exifTimeSeconds

findCloseClusters :: Int -> Int -> [[WatchForStacksFile]] -> ([[WatchForStacksFile]], [[WatchForStacksFile]])
findCloseClusters _ _ [] = ([], [])
findCloseClusters clusterDistance needle (cluster : clusters) =
  let (closeClusters, farClusters) = findCloseClusters clusterDistance needle clusters
      isClose = any (\(WatchForStacksFile _ exifTimeSeconds) -> abs (exifTimeSeconds - needle) < clusterDistance) cluster
   in if isClose
        then (cluster : closeClusters, farClusters)
        else (closeClusters, cluster : farClusters)

insertIntoClusters :: Int -> WatchForStacksFile -> [[WatchForStacksFile]] -> [[WatchForStacksFile]]
insertIntoClusters _ file [] = [[file]]
insertIntoClusters clusterDistance file clusters =
  let (closeClusters, farClusters) = findCloseClusters clusterDistance (wfsfExifTimeSeconds file) clusters
   in (file : concat closeClusters) : farClusters

addFile :: WatchForStacksFile -> WatchForStacksM ()
addFile file@(WatchForStacksFile p exifTimeSeconds) = do
  -- MTL.liftIO $ putStrLn $ "addFile: " ++ p ++ " (" ++ show exifTimeSeconds ++ ")"
  wfss@WatchForStacksState {wfsInFileClusters = clusters, wfsOpts = WatchOptions {optClusterDistance = clusterDistance}} <- MTL.get
  let newClusters = insertIntoClusters clusterDistance file clusters
  MTL.put $ wfss {wfsInFileClusters = newClusters}

addFailedFile :: FilePath -> WatchForStacksM ()
addFailedFile file = do
  wfss@WatchForStacksState {wfsFailedInFiles = failedFiles} <- MTL.get
  MTL.put $ wfss {wfsFailedInFiles = file : failedFiles}

peekFile :: FilePath -> WatchForStacksM ()
peekFile img = do
  WatchForStacksState {wfsExtensions = supportedExtensions} <- MTL.get
  let (_, ext) = splitExtensions img
  if (map toLower ext) `elem` supportedExtensions
    then do
      fileIsAlreadyKnown <- knowsFile img
      if not fileIsAlreadyKnown
        then do
          result <-
            MTL.liftIO $
              catch
                ( do
                    file <- MTL.liftIO $ analyzeFile img
                    return (Just file)
                )
                ( \e -> do
                    putStrLn $ "ERROR: " ++ show (e :: SomeException)
                    return Nothing
                )
          case result of
            Just file -> do
              WatchForStacksState {wfsMinimalTime = wfsMinimalTime} <- MTL.get
              if wfsfExifTimeSeconds file >= wfsMinimalTime
                then addFile file
                else MTL.modify (\s -> s {wfsOldInFiles = img : wfsOldInFiles s})
            Nothing -> addFailedFile img
        else return ()
    else return ()

peekFiles :: WatchForStacksM ()
peekFiles = do
  WatchForStacksState {wfsOpts = WatchOptions {optIndir = indir}} <- MTL.get
  filesInDir <- MTL.liftIO $ getFilesRecursive indir
  MTL.liftIO $ putStrLn $ "#filesInDir: " ++ show (length filesInDir)
  mapM_ peekFile filesInDir

handleFinishedClusters :: WatchForStacksState -> WatchForStacksM () -> WatchForStacksM ()
handleFinishedClusters oldState@WatchForStacksState {wfsInFileClusters = oldClusters} hookAfterImport = do
  let inBox title m = do
        MTL.liftIO . putStrLn $ "▛" ++ (replicate 78 '▀') ++ "▜"
        MTL.liftIO . putStrLn $ "▌ " ++ "Starting " ++ title
        currentTimeBefore <- MTL.liftIO getCurrentTime
        r <- m
        currentTimeAfter <- MTL.liftIO getCurrentTime
        MTL.liftIO . putStrLn $ "▌ " ++ ("Finished " ++ title ++ " (elapsed time: " ++ show (diffUTCTime currentTimeAfter currentTimeBefore) ++ ")")
        MTL.liftIO . putStrLn $ "▙" ++ (replicate 78 '▄') ++ "▟"
        return r
  MTL.liftIO $ putStrLn "handleFinishedClusters ..."
  wfss@WatchForStacksState {wfsOpts = WatchOptions {optOutdir = outdir, optWatchStackOpts = opts}, wfsInFileClusters = newClusters, wfsFinishedClusters = finishedClusters} <- MTL.get
  let (unchanged, changed) = partition (\cluster -> cluster `elem` oldClusters) newClusters

  MTL.liftIO . putStrLn $ "#unchanged: " ++ show (length unchanged)

  MTL.liftIO $ createDirectoryIfMissing True outdir
  newlyImported <-
    mapM
      ( \cluster -> inBox "import" $ do
          let imgs = reverse $ map wfsfPath cluster
          bn <- MTL.liftIO $ getStackOutputBN imgs
          if length cluster > 6
            then do
              MTL.liftIO . putStrLn $ "INFO: work on " ++ bn ++ " of size " ++ show (length cluster)
              let imgs = map wfsfPath cluster
              expectedWD <- MTL.liftIO $ getRawImportDirInWorkdir outdir imgs
              expectedWDExists <- MTL.liftIO $ doesDirectoryExist expectedWD
              if expectedWDExists
                then do
                  MTL.liftIO . putStrLn $ "INFO: img already exists: " ++ expectedWD
                  return (cluster, Nothing)
                else do
                  (wd, importState) <-
                    MTL.liftIO $
                      catch
                        ( do
                            startState <- startMyPhotoState opts imgs
                            (wd, importState) <- runImportStage startState
                            return (wd, Just importState)
                        )
                        ( \e -> do
                            putStrLn $ "ERROR: " ++ show (e :: SomeException)
                            outputBN <- MTL.liftIO $ getStackOutputBN imgs
                            let output = inWorkdir outdir outputBN
                            appendFile (output ++ ".exceptions") (show e ++ "\n")
                            return (output, Nothing)
                        )
                  return (cluster, importState)
            else do
              MTL.liftIO $ do
                putStrLn $ "INFO: cluster too small, just copy: " ++ show (length cluster)
                copy outdir (map wfsfPath cluster)
              return (cluster, Nothing)
      )
      unchanged

  hookAfterImport

  newlyFinished <-
    if optWatchOnlyImport (wfsOpts wfss)
      then return newlyImported
      else
        mapM
          ( \(cluster, mImportState) -> case mImportState of
              Just importState@MyPhotoState {myPhotoStateImgs = imgs} ->
                inBox "stack" $
                  MTL.liftIO $
                    catch
                      ( do
                          (_, stackState) <- runStackStage importState
                          return (cluster, Just stackState)
                      )
                      ( \e -> do
                          outputBN <- MTL.liftIO $ getStackOutputBN imgs
                          let output = inWorkdir outdir outputBN
                          putStrLn $ "ERROR: " ++ show (e :: SomeException)
                          appendFile (output ++ ".exceptions") (show e ++ "\n")
                          return (cluster, Nothing)
                      )
              Nothing -> return (cluster, Nothing)
          )
          newlyImported

  let newFinishedClusters = finishedClusters ++ map (\(b, _) -> b) newlyFinished
  MTL.put $ wfss {wfsInFileClusters = changed, wfsFinishedClusters = newFinishedClusters}

watchForStacksLoop :: WatchForStacksM ()
watchForStacksLoop = do
  MTL.liftIO $ putStrLn "start iteration..."
  oldState <- MTL.get

  -- add new files
  peekFiles

  -- check for finished clusters
  handleFinishedClusters oldState (return ())

  -- log new state
  WatchForStacksState {wfsFailedInFiles = failedFiles, wfsInFileClusters = clusters, wfsFinishedClusters = finishedclusters, wfsOldInFiles = oldFiles} <- MTL.get
  MTL.liftIO $ putStrLn $ "#openClusters: " ++ show (length clusters) ++ "\n##openClusters: " ++ show (map length clusters)
  MTL.liftIO $ putStrLn $ "#finishedClusters: " ++ show (length finishedclusters)
  mapM_ (\cluster -> MTL.liftIO $ putStrLn $ "  #=" ++ show (length cluster)) finishedclusters
  MTL.liftIO $ putStrLn $ "#failedFiles: " ++ show (length failedFiles)
  MTL.liftIO $ putStrLn $ "#oldFiles: " ++ show (length oldFiles)

  -- wait for next loop
  MTL.liftIO $ putStrLn "sleeping..."
  MTL.liftIO $ Thread.threadDelay (loopIntervalInSeconds * 1000000)
  watchForStacksLoop

watchForStacks :: WatchOptions -> IO ()
watchForStacks opts@WatchOptions {..} = do
  putStrLn "watchForStacks"
  indirExists <- doesDirectoryExist optIndir
  unless indirExists $ do
    putStrLn $ "ERROR: indir does not exist: " ++ optIndir
    exitWith (ExitFailure 1)
  putStrLn $ "indir: " ++ optIndir
  putStrLn $ "outdir: " ++ optOutdir

  currentTime <- getCurrentTime
  let currentSeconds = round (utcTimeToPOSIXSeconds currentTime)
  let minimalTime = currentSeconds - optOffset
  let extensions = if optUseRaw then unrawExtensions else jpgExtensions

  MTL.evalStateT watchForStacksLoop (WatchForStacksState opts extensions minimalTime [] [] [] [])

computeInitialState :: WatchOptions -> IO WatchForStacksState
computeInitialState opts@WatchOptions {..} = do
  putStrLn "computeInitialState"
  indirExists <- doesDirectoryExist optIndir
  unless indirExists $ do
    putStrLn $ "ERROR: indir does not exist: " ++ optIndir
    exitWith (ExitFailure 1)
  putStrLn $ "indir: " ++ optIndir
  putStrLn $ "outdir: " ++ optOutdir 

  currentTime <- getCurrentTime
  let currentSeconds = round (utcTimeToPOSIXSeconds currentTime)
  let minimalTime =
        if optOffset > 0
          then currentSeconds - optOffset
          else 0
  let extensions = if optUseRaw then unrawExtensions else jpgExtensions

  return $ WatchForStacksState opts extensions minimalTime [] [] [] []

importStacksOnce :: WatchOptions -> IO ()
importStacksOnce opts@WatchOptions {..} = do
  putStrLn "importStacksOnce"

  initialState <- computeInitialState opts

  MTL.evalStateT
    ( do
        peekFiles
        state <- MTL.get
        handleFinishedClusters state (return ())
    ) initialState

importStacksFromDevice :: WatchOptions -> IO ()
importStacksFromDevice initialOpts = do
  putStrLn "importStacksFromDevice"

  let indevice = optIndir initialOpts

  putStrLn $ "Mounting device: " ++ indevice
  (exitCode, mountPointOutput, stderr) <- readProcessWithExitCode "udisksctl" ["mount", "-b", indevice] ""

  if exitCode /= ExitSuccess
    then do
      putStrLn $ "ERROR: Could not mount device " ++ indevice ++ ": " ++ stderr
      exitWith (ExitFailure 1)
    else return ()

  -- returns e.g. "Mounted /dev/sdb1 at /media/user/XXXX-XXXX"
  let extractMountPoint :: String -> FilePath
      extractMountPoint output =
        let parts = words output
         in if length parts >= 4
              then last parts
              else error $ "Could not extract mount point from udisksctl output: " ++ output
  let mountPoint = extractMountPoint mountPointOutput

  putStrLn $ "Device mounted at: " ++ mountPoint

  let opts =
        initialOpts
          { optIndir = mountPoint </> "DCIM",
            optWatchOnce = True
          }

  initialState <- computeInitialState opts

  let hookAfterImport = MTL.liftIO $ do
        putStrLn "Unmounting device..."
        (exitCode, _, stderr) <- readProcessWithExitCode "udisksctl" ["unmount", "-b", indevice] ""
        if exitCode /= ExitSuccess
          then putStrLn $ "ERROR: Could not unmount device " ++ indevice ++ ": " ++ stderr
          else putStrLn $ "Device unmounted."

  MTL.evalStateT
    ( do
        peekFiles
        state <- MTL.get
        handleFinishedClusters state hookAfterImport
    )
    initialState

getDirs :: [String] -> IO (FilePath, FilePath, [String])
getDirs [] = return (undefined, undefined, ["ERROR: No input directory specified"])
getDirs [indir] = getDirs [indir, "."]
getDirs [indir, outdir] = do
  absIndir <- makeAbsolute indir
  absOutdir <- makeAbsolute outdir
  return (absIndir, absOutdir, [])
getDirs _ = return (undefined, undefined, ["ERROR: Too many arguments specified"])

runMyPhotoWatchForStacks :: IO ()
runMyPhotoWatchForStacks =
  let computeStackOpts :: Options -> [String] -> IO Options
      computeStackOpts initialStackOpts [] = return initialStackOpts
      computeStackOpts initialStackOpts ("--" : stackArgs) = computeStackOpts initialStackOpts stackArgs
      computeStackOpts initialStackOpts stackArgs = do
        let (actions, startImgs, errors) = getOpt RequireOrder options stackArgs
        unless (null errors) $ do
          mapM_ (IO.hPutStrLn IO.stderr) errors
          exitWith (ExitFailure 1)
        unless (null startImgs) $ do
          IO.hPutStrLn IO.stderr "ERROR: No input images expected for watch-for-stacks"
          exitWith (ExitFailure 1)
        foldl (>>=) (return initialStackOpts) actions

      applyStackOpts :: [String] -> WatchOptions -> IO WatchOptions
      applyStackOpts [] opts = return opts
      applyStackOpts ("--" : rest) opts = applyStackOpts rest opts
      applyStackOpts stackArgs opts@WatchOptions {optWatchStackOpts = initialStackOpts} = do
        stackOpts <- computeStackOpts initialStackOpts stackArgs
        return opts {optWatchStackOpts = stackOpts}
   in do
        args <- getArgs
        let (watchArgs, stackArgs) = span (\arg -> arg /= "--") args
        let (actions, nonOptions, errors) = getOpt Permute watchOptions watchArgs
        (indir, outdir, nonOptsErrors) <- getDirs nonOptions
        let errors' = errors ++ nonOptsErrors

        unless (null errors') $ do
          mapM_ (IO.hPutStrLn IO.stderr) errors'
          printHelp
          exitWith (ExitFailure 1)

        let startOpts =
              WatchOptions
                { optWatchVerbose = False,
                  optWatchStackOpts =
                    def
                      { optWorkdirStrategy = ImportToWorkdirWithSubdir outdir,
                        optExport = ExportToParent,
                        optRedirectLog = False
                      },
                  optWatchOnce = False,
                  optWatchOnlyImport = False,
                  optUseRaw = False,
                  optClusterDistance = 6,
                  optOffset = 12 * 60 * 60, -- 12 hours ago
                  optIndir = indir,
                  optOutdir = outdir
                }

        opts <- (foldl (>>=) (return startOpts) actions) >>= applyStackOpts stackArgs

        if ("/dev/" `isPrefixOf` optIndir opts)
          then importStacksFromDevice opts
          else
            if optWatchOnce opts
              then importStacksOnce opts
              else watchForStacks opts
