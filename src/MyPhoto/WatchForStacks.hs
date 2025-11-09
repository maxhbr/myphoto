module MyPhoto.WatchForStacks where

import Control.Concurrent as Thread
import Control.Exception (SomeException, catch)
import Control.Monad ((>=>))
import qualified Control.Monad.State.Lazy as MTL
import Data.Map (Map, fromList, fromListWith, toList, union, unionWith)
import qualified Data.Map as Map
import Data.Time.Clock.POSIX (getCurrentTime, posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import MyPhoto.Actions.FileSystem (copy)
import MyPhoto.Actions.Metadata
import MyPhoto.Actions.UnRAW (unrawExtensions)
import MyPhoto.Model
import MyPhoto.Stack
import System.Console.GetOpt
import System.Directory.Recursive (getFilesRecursive)
import System.Environment (getArgs, getProgName)
import qualified System.IO as IO

data WatchOptions = WatchOptions
  { optWatchVerbose :: Bool,
    optWatchOnce :: Bool,
    optUseRaw :: Bool,
    optOffset :: Int,
    optIndir :: Maybe FilePath,
    optOutdir :: Maybe FilePath
  }
  deriving (Show)

instance Default WatchOptions where
  def =
    WatchOptions
      { optWatchVerbose = False,
        optWatchOnce = False,
        optUseRaw = False,
        optOffset = 12 * 60 * 60, -- 12 hours ago
        optIndir = Nothing,
        optOutdir = Nothing
      }

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
      ["once"]
      ( NoArg
          (\opt -> return opt {optWatchOnce = True})
      )
      "Run once instead of continuous watching",
    Option
      ""
      ["raw"]
      ( NoArg
          (\opt -> return opt {optUseRaw = True})
      )
      "Use RAW files instead of JPG",
    Option
      ""
      ["offset"]
      ( ReqArg
          (\arg opt -> return opt {optOffset = (read arg :: Int) * 60 * 60})
          "HOURS"
      )
      "Time offset in hours (default: 12)",
    Option
      ""
      ["indir"]
      ( ReqArg
          (\arg opt -> return opt {optIndir = Just arg})
          "DIR"
      )
      "Input directory to watch",
    Option
      ""
      ["outdir"]
      ( ReqArg
          (\arg opt -> return opt {optOutdir = Just arg})
          "DIR"
      )
      "Output directory for stacked images",
    Option
      "h"
      ["help"]
      ( NoArg
          ( \_ -> do
              prg <- getProgName
              IO.hPutStr IO.stderr (usageInfo prg watchOptions)
              IO.hPutStrLn IO.stderr ""
              IO.hPutStrLn IO.stderr "Arguments: [INDIR] [OUTDIR]"
              IO.hPutStrLn IO.stderr ""
              IO.hPutStrLn IO.stderr "Examples:"
              IO.hPutStrLn IO.stderr "  myphoto-watch /path/to/input"
              IO.hPutStrLn IO.stderr "  myphoto-watch --once /path/to/input /path/to/output"
              IO.hPutStrLn IO.stderr "  myphoto-watch --raw --offset 12 /path/to/input"
              exitWith ExitSuccess
          )
      )
      "Show help"
  ]

jpgExtensions = [".jpg", ".JPG"]

clusterDistanceInSeconds = 6

loopIntervalInSeconds = 40

data WatchForStacksFile = WatchForStacksFile
  { wfsfPath :: Img,
    -- , wfsfSize :: Int
    -- , wfsfHash :: String
    wfsfExifTimeSeconds :: Int
  }
  deriving (Show, Eq)

data WatchForStacksState = WatchForStacksState
  { wfsExtensions :: [String],
    wfsIndir :: FilePath,
    wfsOutdir :: FilePath,
    wfsMinimalTime :: Int,
    wfsOldInFiles :: [FilePath],
    wfsFailedInFiles :: [FilePath],
    wfsInFileClusters :: [[WatchForStacksFile]],
    wfsFinishedClusters :: Map FilePath [WatchForStacksFile]
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
  let filePathsInFinishedClusters = map wfsfPath (concatMap snd (toList finishedClusters))
  return $ img `elem` (filePathsInInFiles ++ filePathsInFinishedClusters ++ failedFiles ++ oldFiles)

analyzeFile :: FilePath -> IO WatchForStacksFile
analyzeFile p = do
  Metadata {_createDate = exifTimeSeconds} <- getMetadataFromImg False p
  let isoDate = posixSecondsToUTCTime (fromIntegral exifTimeSeconds)
  return $ WatchForStacksFile p exifTimeSeconds

findCloseClusters :: Int -> [[WatchForStacksFile]] -> ([[WatchForStacksFile]], [[WatchForStacksFile]])
findCloseClusters needle [] = ([], [])
findCloseClusters needle (cluster : clusters) =
  let (closeClusters, farClusters) = findCloseClusters needle clusters
      isClose = any (\(WatchForStacksFile _ exifTimeSeconds) -> abs (exifTimeSeconds - needle) < clusterDistanceInSeconds) cluster
   in if isClose
        then (cluster : closeClusters, farClusters)
        else (closeClusters, cluster : farClusters)

insertIntoClusters :: WatchForStacksFile -> [[WatchForStacksFile]] -> [[WatchForStacksFile]]
insertIntoClusters file [] = [[file]]
insertIntoClusters file clusters =
  let (closeClusters, farClusters) = findCloseClusters (wfsfExifTimeSeconds file) clusters
   in (file : concat closeClusters) : farClusters

addFile :: WatchForStacksFile -> WatchForStacksM ()
addFile file@(WatchForStacksFile p exifTimeSeconds) = do
  -- MTL.liftIO $ putStrLn $ "addFile: " ++ p ++ " (" ++ show exifTimeSeconds ++ ")"
  wfss@WatchForStacksState {wfsInFileClusters = clusters} <- MTL.get
  let newClusters = insertIntoClusters file clusters
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
  WatchForStacksState {wfsIndir = indir} <- MTL.get
  filesInDir <- MTL.liftIO $ getFilesRecursive indir
  MTL.liftIO $ putStrLn $ "#filesInDir: " ++ show (length filesInDir)
  mapM_ peekFile filesInDir

handleFinishedClusters :: WatchForStacksState -> WatchForStacksM ()
handleFinishedClusters oldState@(WatchForStacksState {wfsInFileClusters = oldClusters}) = do
  MTL.liftIO $ putStrLn "handleFinishedClusters ..."
  wfss@WatchForStacksState {wfsOutdir = outdir, wfsInFileClusters = newClusters, wfsFinishedClusters = finishedClusters} <- MTL.get
  let (unchanged, changed) = partition (\cluster -> cluster `elem` oldClusters) newClusters

  MTL.liftIO . putStrLn $ "#unchanged: " ++ show (length unchanged)

  MTL.liftIO $ createDirectoryIfMissing True outdir
  newlyFinished <-
    mapM
      ( \cluster -> do
          let imgs = reverse $ map wfsfPath cluster
          let bn = computeStackOutputBN imgs
          if length cluster > 10
            then do
              MTL.liftIO . putStrLn $ "INFO: work on " ++ bn ++ " of size " ++ show (length cluster)
              let imgs = map wfsfPath cluster
              -- for now always assume high mpx, TODO: should be flag or detected
              let highMpxParameters =
                    Map.fromList
                      [ ( "focus-stack",
                          ["--batchsize=6", "--threads=14"]
                        )
                      ]
              let opts =
                    def
                      { optWorkdirStrategy = ImportToWorkdirWithSubdir outdir,
                        optExport = ExportToParent,
                        optRedirectLog = False,
                        optParameters = highMpxParameters
                      }
              let expectedWD = computeRawImportDirInWorkdir outdir imgs
              expectedWDExists <- MTL.liftIO $ doesDirectoryExist expectedWD
              if expectedWDExists
                then do
                  MTL.liftIO . putStrLn $ "INFO: img already exists: " ++ expectedWD
                  return (expectedWD, cluster)
                else do
                  wd <-
                    MTL.liftIO $
                      catch
                        (runMyPhotoStack'' opts [] (map wfsfPath cluster))
                        ( \e -> do
                            let output = outdir </> (computeStackOutputBN imgs)
                            putStrLn $ "ERROR: " ++ show (e :: SomeException)
                            appendFile (output ++ ".exceptions") (show e ++ "\n")
                            return output
                        )
                  return (wd, cluster)
            else do
              MTL.liftIO $ do
                putStrLn $ "INFO: cluster too small, just copy: " ++ show (length cluster)
                copy outdir (map wfsfPath cluster)
              return (outdir, cluster)
      )
      unchanged

  let newFinishedClusters = unionWith (++) finishedClusters (fromListWith (++) newlyFinished)
  MTL.put $ wfss {wfsInFileClusters = changed, wfsFinishedClusters = newFinishedClusters}

watchForStacksLoop :: WatchForStacksM ()
watchForStacksLoop = do
  MTL.liftIO $ putStrLn "start iteration..."
  oldState <- MTL.get

  -- add new files
  peekFiles

  -- check for finished clusters
  handleFinishedClusters oldState

  -- log new state
  WatchForStacksState {wfsFailedInFiles = failedFiles, wfsInFileClusters = clusters, wfsFinishedClusters = finishedclusters, wfsOldInFiles = oldFiles} <- MTL.get
  MTL.liftIO $ putStrLn $ "#openClusters: " ++ show (length clusters) ++ "\n##openClusters: " ++ show (map length clusters)
  MTL.liftIO $ putStrLn $ "#finishedClusters: " ++ show (length finishedclusters)
  mapM_ (\(wd, cluster) -> MTL.liftIO $ putStrLn $ "  " ++ wd ++ " #=" ++ show (length cluster)) (toList finishedclusters)
  MTL.liftIO $ putStrLn $ "#failedFiles: " ++ show (length failedFiles)
  MTL.liftIO $ putStrLn $ "#oldFiles: " ++ show (length oldFiles)

  -- wait for next loop
  MTL.liftIO $ putStrLn "sleeping..."
  MTL.liftIO $ Thread.threadDelay (loopIntervalInSeconds * 1000000)
  watchForStacksLoop

watchForStacks :: Bool -> Int -> FilePath -> FilePath -> IO ()
watchForStacks useRaw offset indir outdir = do
  putStrLn "watchForStacks"
  indirExists <- doesDirectoryExist indir
  unless indirExists $ do
    putStrLn $ "ERROR: indir does not exist: " ++ indir
    exitWith (ExitFailure 1)
  putStrLn $ "indir: " ++ indir
  putStrLn $ "outdir: " ++ outdir

  currentTime <- getCurrentTime
  let currentSeconds = round (utcTimeToPOSIXSeconds currentTime)
  let minimalTime = currentSeconds - offset
  let extensions = if useRaw then unrawExtensions else jpgExtensions

  MTL.evalStateT watchForStacksLoop (WatchForStacksState extensions indir outdir minimalTime [] [] [] mempty)

importStacksOnce :: Bool -> Int -> FilePath -> FilePath -> IO ()
importStacksOnce useRaw offset indir outdir = do
  putStrLn "importStacksOnce"
  indirExists <- doesDirectoryExist indir
  unless indirExists $ do
    putStrLn $ "ERROR: indir does not exist: " ++ indir
    exitWith (ExitFailure 1)
  putStrLn $ "indir: " ++ indir
  putStrLn $ "outdir: " ++ outdir

  currentTime <- getCurrentTime
  let currentSeconds = round (utcTimeToPOSIXSeconds currentTime)
  let minimalTime = currentSeconds - offset
  let extensions = if useRaw then unrawExtensions else jpgExtensions

  MTL.evalStateT
    ( do
        peekFiles
        state <- MTL.get
        handleFinishedClusters state
    )
    (WatchForStacksState extensions indir outdir minimalTime [] [] [] mempty)


finalizeOpts :: [String] -> WatchOptions -> IO WatchOptions
finalizeOpts nonOptions = let
    parsePositionalDirs opts = case (optIndir opts, optOutdir opts, nonOptions) of
          (Nothing, Nothing, []) -> return opts -- No directories specified, will fail later
          (Nothing, Nothing, [indir]) -> return $ opts {optIndir = Just indir, optOutdir = Just "."}
          (Nothing, Nothing, [indir, outdir]) -> return $ opts {optIndir = Just indir, optOutdir = Just outdir}
          (Nothing, Nothing, _) -> return opts -- Too many arguments, will fail later
          (Just _, Nothing, []) -> return $ opts {optOutdir = Just "."}
          (Just _, Nothing, [outdir]) -> return $ opts {optOutdir = Just outdir}
          (Just _, Just _, []) -> return opts -- Both already set via options
          (Nothing, Just _, [indir]) -> return $ opts {optIndir = Just indir}
          _ -> return opts -- Invalid combination
    makeDirsAbsolute opts@WatchOptions{optIndir = indir, optOutdir = outdir} = do
      absIndir <- traverse makeAbsolute indir
      absOutdir <- traverse makeAbsolute outdir
      return $ opts {optIndir = absIndir, optOutdir = absOutdir} 
  in parsePositionalDirs >=> makeDirsAbsolute

runMyPhotoWatchForStacks :: IO ()
runMyPhotoWatchForStacks = do
  args <- getArgs
  let (actions, nonOptions, errors) = getOpt Permute watchOptions args
  
  unless (null errors) $ do
    mapM_ (IO.hPutStrLn IO.stderr) errors
    exitWith (ExitFailure 1)
  
  opts <- foldl (>>=) (return def) actions

  finalOpts <- finalizeOpts nonOptions opts
  case optIndir finalOpts of
    Nothing -> do
      IO.hPutStrLn IO.stderr "ERROR: No input directory specified"
      prg <- getProgName
      IO.hPutStrLn IO.stderr (usageInfo prg watchOptions)
      exitWith (ExitFailure 1)
    Just indir -> do
      let outdir = case optOutdir finalOpts of
            Nothing -> "."
            Just od -> od
      
      let finalOffset = if optUseRaw finalOpts && optOffset finalOpts == 12 * 60 * 60
                        then 12 * 60 * 60 
                        else optOffset finalOpts
      
      if optWatchOnce finalOpts
        then importStacksOnce (optUseRaw finalOpts) finalOffset indir outdir
        else watchForStacks (optUseRaw finalOpts) finalOffset indir outdir
