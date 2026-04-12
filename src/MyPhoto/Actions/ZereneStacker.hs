module MyPhoto.Actions.ZereneStacker
  ( zereneStackerImgs,
    zereneStackerImgsParallel,
  )
where

import Control.Concurrent.Async (concurrently)
import qualified Control.Concurrent.MSem as MS
import Data.Maybe (catMaybes)
import GHC.Conc (getNumCapabilities)
import MyPhoto.Model
import MyPhoto.Utils.Chunking
import MyPhoto.Wrapper.ZereneStackerWrapper
import System.Directory (renameFile)

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

-- | Run a single pass of Zerene Stacker on a chunk of images, producing one output file.
-- Used as the leaf function for resolveChunks.
zereneChunkLeaf :: Bool -> Bool -> FilePath -> Maybe FilePath -> Maybe FilePath -> FilePath -> Imgs -> IO (Either String Img)
zereneChunkLeaf headless verbose workdir pmaxOutput dmapOutput _outputBN imgs = do
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
      let opts =
            ZereneStackerOptions
              { _Headless = headless,
                _Wait = False,
                _Verbose = verbose,
                _Align = False, -- chunking only allowed when already aligned
                _PMaxOutput = pmaxOutput,
                _DMapOutput = dmapOutput,
                _Cwd = Just workdir
              }
      runZereneStacker opts imgs
      return (Right outFile)

-- | Run Zerene Stacker with chunking support.
-- Runs two passes: one for PMax, one for DMap.
zereneStackerChunked :: Bool -> Bool -> ChunkSettings -> FilePath -> [FilePath] -> IO (Either String [FilePath])
zereneStackerChunked headless verbose chunkSettings outputBN imgs = do
  pmaxOutput' <- makeAbsolute (outputBN ++ "_zerene-PMax.tif")
  dmapOutput' <- makeAbsolute (outputBN ++ "_zerene-DMap.tif")

  pmaxOutput <- fromFilePath pmaxOutput'
  dmapOutput <- fromFilePath dmapOutput'

  workdir <- makeAbsolute (outputBN ++ "_zerene-stacker.workdir")
  createDirectoryIfMissing True workdir
  let bnInWorkdir = workdir </> takeFileName outputBN

  numCapabilities <- getNumCapabilities
  let numThreadsFactor = 8
  let numThreads = max 1 (ceiling (fromIntegral numCapabilities / numThreadsFactor :: Double))
  sem <- MS.new numThreads

  let chunks = mkChunks chunkSettings imgs
  logInfoIO ("Zerene Stacker chunked: " ++ showChunkTree chunks)

  -- Pass 1: PMax
  when (isTodo pmaxOutput) $ do
    logInfoIO "Zerene Stacker chunked: starting PMax pass"
    let pmaxBN = bnInWorkdir ++ "_zerene-PMax"
    let pmaxLayersTiff = pmaxOutput' -<.> "layers.tif"
    pmaxResult <-
      resolveChunksAndSaveLayersTiff
        sem
        ( \bn' imgs' -> do
            let outFile = bn' ++ ".tif"
            zereneChunkLeaf headless verbose workdir (Just outFile) Nothing bn' imgs'
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
    let dmapBN = bnInWorkdir ++ "_zerene-DMap"
    let dmapLayersTiff = dmapOutput' -<.> "layers.tif"
    dmapResult <-
      resolveChunksAndSaveLayersTiff
        sem
        ( \bn' imgs' -> do
            let outFile = bn' ++ ".tif"
            zereneChunkLeaf headless verbose workdir Nothing (Just outFile) bn' imgs'
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

zereneStackerImgs :: Bool -> Bool -> Bool -> ChunkSettings -> FilePath -> [FilePath] -> IO (Either String [FilePath])
zereneStackerImgs headless verbose align chunkSettings outputBN imgs =
  case chunkSettings of
    NoChunks -> zereneStackerImgsNoChunks headless verbose align outputBN imgs
    _ -> zereneStackerChunked headless verbose chunkSettings outputBN imgs

-- | Original non-chunked implementation.
zereneStackerImgsNoChunks :: Bool -> Bool -> Bool -> FilePath -> [FilePath] -> IO (Either String [FilePath])
zereneStackerImgsNoChunks headless verbose align outputBN imgs = do
  pmaxOutput' <- makeAbsolute (outputBN ++ "_zerene-PMax.tif")
  dmapOutput' <- makeAbsolute (outputBN ++ "_zerene-DMap.tif")

  pmaxOutput <- fromFilePath pmaxOutput'
  dmapOutput <- fromFilePath dmapOutput'

  workdir <- makeAbsolute (outputBN ++ "_zerene-stacker.workdir")
  createDirectoryIfMissing True workdir

  if not (isTodo pmaxOutput) && not (isTodo dmapOutput)
    then do
      logInfoIO ("Zerene Stacker outputs " ++ show pmaxOutput ++ " and " ++ show dmapOutput ++ " already exist, check that all aligned images are present")
    else do
      let opts =
            ZereneStackerOptions
              { _Headless = headless,
                _Wait = False,
                _Verbose = verbose,
                _Align = align,
                _PMaxOutput = toOpts pmaxOutput,
                _DMapOutput = toOpts dmapOutput,
                _Cwd = Just workdir
              }
      runZereneStacker opts imgs

  return (Right (catMaybes [toResult pmaxOutput, toResult dmapOutput]))

-- | Run PMax and DMap as two separate Zerene Stacker processes in parallel.
zereneStackerImgsParallel :: Bool -> Bool -> ChunkSettings -> FilePath -> [FilePath] -> IO (Either String [FilePath])
zereneStackerImgsParallel verbose align chunkSettings outputBN imgs =
  case chunkSettings of
    NoChunks -> zereneStackerImgsParallelNoChunks verbose align outputBN imgs
    _ -> zereneStackerChunked True verbose chunkSettings outputBN imgs

-- | Original non-chunked parallel implementation.
zereneStackerImgsParallelNoChunks :: Bool -> Bool -> FilePath -> [FilePath] -> IO (Either String [FilePath])
zereneStackerImgsParallelNoChunks verbose align outputBN imgs = do
  pmaxOutput' <- makeAbsolute (outputBN ++ "_zerene-PMax.tif")
  dmapOutput' <- makeAbsolute (outputBN ++ "_zerene-DMap.tif")

  pmaxOutput <- fromFilePath pmaxOutput'
  dmapOutput <- fromFilePath dmapOutput'

  workdir <- makeAbsolute (outputBN ++ "_zerene-stacker.workdir")
  createDirectoryIfMissing True workdir

  let runPMax =
        if isTodo pmaxOutput
          then do
            logInfoIO ("Zerene Stacker parallel: starting PMax")
            runZereneStacker
              ZereneStackerOptions
                { _Headless = True,
                  _Wait = False,
                  _Verbose = verbose,
                  _Align = align,
                  _PMaxOutput = toOpts pmaxOutput,
                  _DMapOutput = Nothing,
                  _Cwd = Just workdir
                }
              imgs
            logInfoIO ("Zerene Stacker parallel: PMax done")
          else logInfoIO ("Zerene Stacker parallel: PMax output already exists, skipping")

      runDMap =
        if isTodo dmapOutput
          then do
            logInfoIO ("Zerene Stacker parallel: starting DMap")
            runZereneStacker
              ZereneStackerOptions
                { _Headless = True,
                  _Wait = False,
                  _Verbose = verbose,
                  _Align = align,
                  _PMaxOutput = Nothing,
                  _DMapOutput = toOpts dmapOutput,
                  _Cwd = Just workdir
                }
              imgs
            logInfoIO ("Zerene Stacker parallel: DMap done")
          else logInfoIO ("Zerene Stacker parallel: DMap output already exists, skipping")

  _ <- concurrently runPMax runDMap

  return (Right (catMaybes [toResult pmaxOutput, toResult dmapOutput]))
