module MyPhoto.WatchForStacks where



import MyPhoto.Model
import MyPhoto.Actions.Metadata
import qualified Control.Monad.State.Lazy as MTL
import Control.Concurrent as Thread
import System.Directory.Recursive (getFilesRecursive)
import System.Environment (getArgs)

import Data.Map (Map)

supportedExtensions = [".jpg", ".JPG"]
clusterDistanceInSeconds = 6

data WatchForStacksFile
  = WatchForStacksFile
  { wfsfPath :: Img
  -- , wfsfSize :: Int
  -- , wfsfHash :: String
  , wfsfExifTimeSeconds :: Int
  }

data WatchForStacksState
  = WatchForStacksState
  { wfsIndir :: FilePath
  , wfsOutdir :: FilePath
  , wfsInFileClusters :: [[WatchForStacksFile]]
  }

type WatchForStacksM = MTL.StateT WatchForStacksState IO

knowsFile :: FilePath -> WatchForStacksM Bool
knowsFile img = do
  WatchForStacksState indir outdir inFiles <- MTL.get
  return $ img `elem` (map wfsfPath (concat inFiles))

analyzeFile :: FilePath -> IO WatchForStacksFile
analyzeFile p = do
  Metadata _ exifTimeSeconds <- getMetadaFromImg False p
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
addFile file = do
  WatchForStacksState indir outdir clusters <- MTL.get
  let newClusters = insertIntoClusters file clusters
  MTL.put $ WatchForStacksState indir outdir newClusters

peekFile :: FilePath -> WatchForStacksM ()
peekFile img = do
  let (_, ext) = splitExtensions img
  if ext `elem` supportedExtensions
    then do
      fileIsAlreadyKnown <- knowsFile img
      if not fileIsAlreadyKnown
        then do
          file <- MTL.liftIO $ analyzeFile img
          addFile file
        else return ()
    else return ()
  
watchForStacksLoop :: WatchForStacksM ()
watchForStacksLoop = do
  WatchForStacksState indir _ _<- MTL.get
  filesInDir <- MTL.liftIO $ getFilesRecursive indir
  MTL.liftIO $ putStrLn $ "#filesInDir: " ++ show (length filesInDir)

  mapM_ peekFile filesInDir

  WatchForStacksState _ _ clusters <- MTL.get
  MTL.liftIO $ putStrLn $ "#clusters: " ++ show (length clusters) ++ " ##clusters: " ++ show (map length clusters)
  MTL.liftIO $ Thread.threadDelay 60000000
  watchForStacksLoop

watchForStacks :: FilePath -> FilePath -> IO ()
watchForStacks indir outdir = do
  putStrLn "watchForStacks"
  putStrLn $ "indir: " ++ indir
  putStrLn $ "outdir: " ++ outdir
  
  MTL.evalStateT watchForStacksLoop (WatchForStacksState indir outdir [])


runMyPhotoWatchForStacks :: IO ()
runMyPhotoWatchForStacks = do
  let help = do
        putStrLn "Usage: my-photo-watch-for-stacks <indir>"
        putStrLn "Usage: my-photo-watch-for-stacks <indir> <outdir>"
  args <- getArgs
  case args of
    ["-h"] -> do
      help
      exitWith ExitSuccess
    ["--help"] -> do
      help
      exitWith ExitSuccess
    [indir] -> watchForStacks indir "."
    [indir, outdir] -> watchForStacks indir outdir
    _ -> do
      help
      exitWith (ExitFailure 1)
