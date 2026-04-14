module MyPhoto.Actions.ZereneStacker
  ( zereneStacker,
    ZereneStackerActionOptions (..),
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

data ZereneStackerActionOptions = ZereneStackerActionOptions
  { zsVerbose :: Bool,
    zsHeadless :: Bool,
    zsParallel :: Bool,
    zsAlign :: Bool,
    zsChunkSettings :: ChunkSettings
  }
  deriving (Show)

instance Default ZereneStackerActionOptions where
  def =
    ZereneStackerActionOptions
      { zsVerbose = False,
        zsHeadless = False,
        zsParallel = False,
        zsAlign = True,
        zsChunkSettings = NoChunks
      }

fixZereneStackerActionOptions :: ZereneStackerActionOptions -> IO ZereneStackerActionOptions
fixZereneStackerActionOptions opts@ZereneStackerActionOptions {zsHeadless = True, zsParallel = True} = do
  logWarnIO "Zerene Stacker: headless mode does not support parallel execution, disabling parallel mode"
  fixZereneStackerActionOptions (opts {zsParallel = False})
fixZereneStackerActionOptions opts@ZereneStackerActionOptions {zsAlign = False, zsChunkSettings = cs} =
  if cs /= NoChunks
    then do
      logWarnIO "Zerene Stacker: chunking does not support --already-aligned, disabling chunking"
      fixZereneStackerActionOptions (opts {zsChunkSettings = NoChunks})
    else return opts
fixZereneStackerActionOptions opts = return opts

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
zereneStacker :: ZereneStackerActionOptions -> FilePath -> [FilePath] -> IO (Either String [FilePath])
zereneStacker opts outputBN imgs = do
  workdir <- makeAbsolute (outputBN ++ "_zerene-stacker.workdir")
  createDirectoryIfMissing True workdir
  case zsChunkSettings opts of
    NoChunks -> zereneStackerImgsNoChunks opts workdir outputBN imgs
    _ -> zereneStackerChunked opts workdir outputBN imgs

-- | Run a single pass of Zerene Stacker on a chunk of images, producing one output file.
-- Used as the leaf function for resolveChunks.
zereneChunkLeaf :: ZereneStackerActionOptions -> Maybe FilePath -> Maybe FilePath -> FilePath -> Imgs -> IO (Either String FilePath)
zereneChunkLeaf opts pmaxOutput dmapOutput _outputBN imgs = do
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
              { _Headless = zsHeadless opts,
                _Wait = False,
                _Verbose = zsVerbose opts,
                _Align = False, -- chunking only allowed when already aligned
                _PMaxOutput = pmaxOutput,
                _DMapOutput = dmapOutput,
                _Cwd = Just workdir
              }
      runZereneStacker wrapperOpts imgs
      return (Right outFile)

-- | Run Zerene Stacker with chunking support.
-- Runs two passes: one for PMax, one for DMap.
zereneStackerChunked :: ZereneStackerActionOptions -> FilePath -> FilePath -> [FilePath] -> IO (Either String [FilePath])
zereneStackerChunked opts workdir outputBN imgs = do
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

  let chunks = mkChunks (zsChunkSettings opts) imgs
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
            zereneChunkLeaf opts (Just outFile) Nothing bn' imgs'
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
            zereneChunkLeaf opts Nothing (Just outFile) bn' imgs'
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
zereneStackerImgsNoChunks :: ZereneStackerActionOptions -> FilePath -> FilePath -> [FilePath] -> IO (Either String [FilePath])
zereneStackerImgsNoChunks opts workdir outputBN imgs = do
  pmaxOutput' <- makeAbsolute (outputBN ++ "_zerene-PMax.tif")
  dmapOutput' <- makeAbsolute (outputBN ++ "_zerene-DMap.tif")

  pmaxOutput <- fromFilePath pmaxOutput'
  dmapOutput <- fromFilePath dmapOutput'

  let mkZerenStackerOptions pmax dmap =
        ZereneStackerOptions
          { _Headless = zsHeadless opts,
            _Wait = False,
            _Verbose = zsVerbose opts,
            _Align = zsAlign opts,
            _PMaxOutput = pmax,
            _DMapOutput = dmap,
            _Cwd = Just workdir
          }

  if not (isTodo pmaxOutput) && not (isTodo dmapOutput)
    then do
      logInfoIO ("Zerene Stacker outputs " ++ show pmaxOutput ++ " and " ++ show dmapOutput ++ " already exist, check that all aligned images are present")
    else
      if zsParallel opts
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
