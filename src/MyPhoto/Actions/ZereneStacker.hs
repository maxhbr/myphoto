module MyPhoto.Actions.ZereneStacker
  ( zereneStacker,
    ZereneStackerMode (..),
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently)
import qualified Control.Concurrent.MSem as MS
import Data.Maybe (catMaybes)
import GHC.Conc (getNumCapabilities)
import MyPhoto.Model
import MyPhoto.Utils.Chunking
import MyPhoto.Wrapper.ZereneStackerWrapper
import System.Directory (renameFile)

data ZereneStackerMode
  = ZereneStackerDefault Bool
  | ZereneStackerHeadless Bool
  | ZereneStackerParallel
  | ZereneStackerChunked (ChunkSettings)
  deriving (Show, Eq)

instance Default ZereneStackerMode where
  def = ZereneStackerDefault False

shouldAlign :: ZereneStackerMode -> Bool
shouldAlign (ZereneStackerHeadless align) = align
shouldAlign ZereneStackerParallel = False
shouldAlign (ZereneStackerChunked _) = False
shouldAlign (ZereneStackerDefault align) = align

data ZereneStackerImagePlan
  = Planned FilePath
  | Done FilePath
  | NotPlanned
  deriving (Show, Eq)

fromFilePath :: FilePath -> IO ZereneStackerImagePlan
fromFilePath fp = do
  exists <- doesFileExist fp
  return $
    if exists
      then Done fp
      else Planned fp

toOpts :: ZereneStackerImagePlan -> Maybe FilePath
toOpts (Planned fp) = Just fp
toOpts (Done _) = Nothing
toOpts NotPlanned = Nothing

toResult :: ZereneStackerImagePlan -> Maybe FilePath
toResult (Planned fp) = Just fp
toResult (Done fp) = Just fp
toResult NotPlanned = Nothing

isTodo :: ZereneStackerImagePlan -> Bool
isTodo (Planned _) = True
isTodo (Done _) = False
isTodo NotPlanned = False

-- | Main entrypoint for Zerene Stacker operations.
zereneStacker :: Bool -> ZereneStackerMode -> FilePath -> [FilePath] -> IO (Either String [FilePath])
zereneStacker verbose mode outputBN imgs = do
  logInfoIO ("Running Zerene Stacker with mode " ++ show mode)
  workdir <- makeAbsolute (outputBN ++ "_zerene-stacker.workdir")
  createDirectoryIfMissing True workdir
  case mode of
    ZereneStackerDefault align -> zereneStackerImgsNoChunks verbose False False align workdir outputBN imgs
    ZereneStackerHeadless align -> zereneStackerImgsNoChunks verbose True False align workdir outputBN imgs
    ZereneStackerParallel -> zereneStackerImgsNoChunks verbose False True False workdir outputBN imgs
    ZereneStackerChunked chunkSettings -> zereneStackerChunked verbose chunkSettings workdir outputBN imgs


-- | Run a single pass of Zerene Stacker on a chunk of images, producing one output file.
-- Used as the leaf function for resolveChunks.
zereneChunkLeaf :: Bool -> Maybe FilePath -> Maybe FilePath -> FilePath -> Imgs -> IO (Either String FilePath)
zereneChunkLeaf verbose pmaxOutput dmapOutput _outputBN imgs = do
  let workdir = takeDirectory _outputBN
  let outFile = case (pmaxOutput, dmapOutput) of
        (Just fp, _) -> fp
        (_, Just fp) -> fp
        _ -> error "zereneChunkLeaf: no output specified"
  outFileExists <- doesFileExist outFile
  if outFileExists
    then do
      logInfoIO ("#### " ++ outFile ++ " already exists, skipping")
      return (Right outFile)
    else do
      let wrapperOpts =
            ZereneStackerOptions
              { _Headless = False, 
                _Wait = False,
                _Verbose = verbose,
                _Align = False, -- chunking only allowed when already aligned
                _PMaxOutput = pmaxOutput,
                _DMapOutput = dmapOutput,
                _Cwd = Just workdir
              }
      runZereneStacker wrapperOpts imgs
      return (Right outFile)

-- | Run Zerene Stacker with chunking support.
-- Runs two passes: one for PMax, one for DMap.
zereneStackerChunked :: Bool -> ChunkSettings -> FilePath -> FilePath -> [FilePath] -> IO (Either String [FilePath])
zereneStackerChunked verbose chunkSettings workdir outputBN imgs = do
  pmaxOutput' <- makeAbsolute (outputBN ++ "_zerene-PMax-Chunked.tif")
  dmapOutput' <- makeAbsolute (outputBN ++ "_zerene-DMap-Chunked.tif")

  let pmaxBN = workdir </> takeBaseName pmaxOutput'
      dmapBN = workdir </> takeBaseName dmapOutput'

  pmaxOutput <- fromFilePath pmaxOutput'
  dmapOutput <- fromFilePath dmapOutput'

  numCapabilities <- getNumCapabilities
  let numThreadsFactor = 8
  let numThreads = max 1 (ceiling (fromIntegral numCapabilities / numThreadsFactor :: Double))
  sem <- MS.new numThreads

  let chunks = mkChunks chunkSettings imgs
  logInfoIO ("Zerene Stacker chunked: " ++ showChunkTree chunks)

  -- Pass 1: PMax
  when (isTodo pmaxOutput) $ do
    logInfoIO "Zerene Stacker chunked: starting PMax pass"
    let pmaxLayersTiff = pmaxOutput' -<.> "layers.tif"
    pmaxResult <-
      resolveChunksAndSaveLayersTiff
        sem
        ( \bn' imgs' -> do
            let outFile = bn' ++ ".tif"
            zereneChunkLeaf verbose (Just outFile) Nothing bn' imgs'
        )
        pmaxBN
        chunks
        pmaxLayersTiff
    case pmaxResult of
      Right generatedFile -> do
        logDebugIO ("Zerene Stacker chunked: renaming " ++ generatedFile ++ " to " ++ pmaxOutput')
        renameFile generatedFile pmaxOutput'
        logInfoIO "Zerene Stacker chunked: PMax pass done"
      Left err -> logErrorIO ("Zerene Stacker chunked: PMax pass failed: " ++ err)

  -- Pass 2: DMap
  when (isTodo dmapOutput) $ do
    logInfoIO "Zerene Stacker chunked: starting DMap pass"
    let dmapLayersTiff = dmapOutput' -<.> "layers.tif"
    dmapResult <-
      resolveChunksAndSaveLayersTiff
        sem
        ( \bn' imgs' -> do
            let outFile = bn' ++ ".tif"
            zereneChunkLeaf verbose Nothing (Just outFile) bn' imgs'
        )
        dmapBN
        chunks
        dmapLayersTiff
    case dmapResult of
      Right generatedFile -> do
        logDebugIO ("Zerene Stacker chunked: renaming " ++ generatedFile ++ " to " ++ dmapOutput')
        renameFile generatedFile dmapOutput'
        logInfoIO "Zerene Stacker chunked: DMap pass done"
      Left err -> logErrorIO ("Zerene Stacker chunked: DMap pass failed: " ++ err)

  return (Right (catMaybes [toResult pmaxOutput, toResult dmapOutput]))

-- | Original non-chunked implementation.
zereneStackerImgsNoChunks :: Bool -> Bool -> Bool -> Bool -> FilePath -> FilePath -> [FilePath] -> IO (Either String [FilePath])
zereneStackerImgsNoChunks verbose parallel headless align workdir outputBN imgs = do
  pmaxOutput' <- makeAbsolute (outputBN ++ "_zerene-PMax.tif")
  dmapOutput' <- makeAbsolute (outputBN ++ "_zerene-DMap.tif")

  pmaxOutput <- fromFilePath pmaxOutput'
  dmapOutput <- fromFilePath dmapOutput'

  let mkZerenStackerOptions pmax dmap =
        ZereneStackerOptions
          { _Headless = headless,
            _Wait = False,
            _Verbose = verbose,
            _Align = align,
            _PMaxOutput = pmax,
            _DMapOutput = dmap,
            _Cwd = Just workdir
          }

  if not (isTodo pmaxOutput) && not (isTodo dmapOutput)
    then do
      logInfoIO ("Zerene Stacker outputs " ++ show pmaxOutput ++ " and " ++ show dmapOutput ++ " already exist, check that all aligned images are present")
    else
      if parallel
        then do
          let runPMax =
                if isTodo pmaxOutput
                  then do
                    logInfoIO ("Zerene Stacker parallel: starting PMax")
                    runZereneStacker (mkZerenStackerOptions (toOpts pmaxOutput) Nothing) imgs
                    logInfoIO ("Zerene Stacker parallel: PMax done")
                  else logInfoIO ("Zerene Stacker parallel: PMax output already exists, skipping")
              runDMap =
                if isTodo dmapOutput
                  then do
                    logInfoIO ("Zerene Stacker parallel: starting DMap")
                    runZereneStacker (mkZerenStackerOptions Nothing (toOpts dmapOutput)) imgs
                    logInfoIO ("Zerene Stacker parallel: DMap done")
                  else logInfoIO ("Zerene Stacker parallel: DMap output already exists, skipping")

          logInfoIO "Zerene Stacker parallel: starting parallel execution"
          _ <- concurrently (threadDelay (1 * 1000) >> runPMax) (threadDelay (2 * 1000) >> runDMap)
          return ()
        else
          runZereneStacker (mkZerenStackerOptions (toOpts pmaxOutput) (toOpts dmapOutput)) imgs

  return (Right (catMaybes [toResult pmaxOutput, toResult dmapOutput]))
