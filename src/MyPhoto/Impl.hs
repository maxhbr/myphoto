{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MyPhoto.Impl
  ( runImportStage,
    runStackStage,
    getRawImportDirInWorkdir,
    earlyExport,
  )
where

import Control.Concurrent (getNumCapabilities)
import qualified Control.Exception as Ex
import Control.Monad (guard)
import qualified Control.Monad.State.Lazy as MTL
import Data.List (isInfixOf)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Data.Text.Array (run)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Format (defaultTimeLocale, formatTime)
import MyPhoto.Actions.Align
import MyPhoto.Actions.Downscale
import MyPhoto.Actions.EnblendEnfuse
import MyPhoto.Actions.FileSystem
import MyPhoto.Actions.FocusStack
import MyPhoto.Actions.Metadata
import MyPhoto.Actions.Montage
import MyPhoto.Actions.Outliers
import MyPhoto.Actions.UnHeif
import MyPhoto.Actions.UnRAW
import MyPhoto.Actions.UnTiff
import MyPhoto.Actions.ZereneStacker (ZereneStackerMode (..), zereneStacker)
import MyPhoto.Model
import MyPhoto.Monad
import MyPhoto.Video
import System.Console.GetOpt
import System.Directory (executable, getPermissions, removeDirectoryRecursive, setPermissions)
import System.Environment (getArgs, getProgName, withArgs)
import qualified System.IO as IO
import System.Process (createProcess, proc, waitForProcess)

getStackOutputBNFromImgs :: MyPhotoM FilePath
getStackOutputBNFromImgs = do
  imgs <- getImgs
  MTL.liftIO $ getStackOutputBN imgs

getRawImportDirInWorkdir :: FilePath -> Imgs -> IO FilePath
getRawImportDirInWorkdir wd imgs = do
  outputBN <- getStackOutputBN imgs
  return $ inWorkdir wd (outputBN <.> "raw")

openLogFileForImgs :: MyPhotoM ()
openLogFileForImgs = do
  outputBN <- getStackOutputBNFromImgs
  wd <- MTL.gets myPhotoStateWd
  case wd of
    Nothing -> return ()
    Just wd' -> do
      opts <- getOpts
      let logDir = case optExport opts of
            ExportToParent -> takeDirectory wd'
            _ -> wd'
          logFile = logDir </> (outputBN <.> "myphoto.log")
      openLogFileAt logFile
      logDebug ("Options: " ++ show opts)

getWdOrFail :: MyPhotoM FilePath
getWdOrFail = do
  wd <- MTL.gets myPhotoStateWd
  case wd of
    Just wd -> return wd
    Nothing -> error "working directory not yet set"

getWdAndMaybeMoveImgs :: MyPhotoM FilePath
getWdAndMaybeMoveImgs =
  let
   in -- getImgsManifest :: MyPhotoM String
      -- getImgsManifest = do
      --   imgs <- getImgs
      --   return (show imgs)
      -- compareImgsManifest :: String -> FilePath -> MyPhotoM Bool
      -- compareImgsManifest manifest wd = do
      --   let oldManifestfile = wd </> ".myphoto_imgs_manifest"
      --   oldManifest <- MTL.liftIO $ Ex.try (IO.readFile oldManifestfile) :: MyPhotoM (Either Ex.IOException String)
      --   case oldManifest of
      --     Left _ -> return False
      --     Right oldManifestContent -> return (oldManifestContent == manifest)
      -- writeImgsManifest :: String -> FilePath -> MyPhotoM ()
      -- writeImgsManifest manifest wd = do
      --   MTL.liftIO $ IO.writeFile (wd </> ".myphoto_imgs_manifest") manifest
      do
        wd <- MTL.gets myPhotoStateWd
        case wd of
          Just wd -> return wd
          Nothing -> do
            logDebug ("determine working directory")
            Options {optWorkdirStrategy = workdirStrategy} <- getOpts
            let implForImportToWorkdir wd = do
                  Options {optEveryNth = everyNth} <- getOpts
                  when (isJust everyNth) $ do
                    fail "cannot import images to subfolder when --every-nth is specified"
                  MTL.liftIO $ createDirectoryIfMissing True wd
                  absWd <- MTL.liftIO $ makeAbsolute wd
                  imgs <- getImgs
                  indir <- MTL.liftIO $ getRawImportDirInWorkdir absWd imgs
                  let expectedFiles = map (inWorkdir indir) imgs
                  allFilesExist <- and <$> mapM (MTL.liftIO . doesFileExist) expectedFiles
                  imgs' <-
                    if allFilesExist
                      then do
                        logInfo ("importToWorkdir: skipping copy of images to " ++ indir ++ " as they already exist there")
                        return expectedFiles
                      else step ("copy images to " ++ indir) $ MTL.liftIO $ copy indir imgs
                  putImgs imgs'
                  return absWd

            wd <- case workdirStrategy of
              CreateNextToImgDir -> do
                outputBN <- getStackOutputBNFromImgs
                img0 : _ <- getImgs
                let wd = (inWorkdir (takeDirectory img0) outputBN) <.> "myphoto"
                MTL.liftIO $ createDirectoryIfMissing True wd
                return wd
              MoveExistingImgsToSubfolder -> do
                opts <- getOpts
                when (isJust (optEveryNth opts)) $ do
                  fail "cannot move images to subfolder when --every-nth is specified"
                imgs@(img0 : _) <- getImgs
                let wd = takeDirectory img0
                outputBN <- getStackOutputBNFromImgs
                let indir = inWorkdir wd (outputBN <.> "raw")
                logInfo ("moving images to " ++ indir)
                imgs' <- MTL.liftIO $ move indir imgs
                putImgs imgs'
                return wd
              NextToImgFiles -> do
                (img0 : _) <- getImgs
                let wd = takeDirectory img0
                return wd
              WorkdirStrategyOverwrite wd -> do
                MTL.liftIO $ createDirectoryIfMissing True wd
                MTL.liftIO $ makeAbsolute wd
              ImportToWorkdir wd -> implForImportToWorkdir wd
              ImportToWorkdirWithSubdir wd -> do
                outputBN <- getStackOutputBNFromImgs
                let wdWithSubdir = inWorkdir wd (outputBN <.> "myphoto")
                implForImportToWorkdir wdWithSubdir
            setWd wd
            return wd

readDirectoryIfOnlyOneWasSpecified :: MyPhotoM ()
readDirectoryIfOnlyOneWasSpecified = do
  imgs <- getImgs
  case imgs of
    [maybeDir] -> do
      isExistingDirectory <- MTL.liftIO $ doesDirectoryExist maybeDir
      when isExistingDirectory $ do
        logInfo ("directory specified: " ++ maybeDir)
        imgs' <- MTL.liftIO $ map (maybeDir </>) <$> listDirectory maybeDir
        logInfo ("#images: " ++ show (length imgs'))
        putImgs imgs'
    _ -> return ()

failIfNoImagesWereSpecified :: MyPhotoM ()
failIfNoImagesWereSpecified = do
  imgs <- getImgs
  when (null imgs) $ MTL.liftIO $ do
    IO.hPutStrLn IO.stderr "no image specified"
    exitWith (ExitFailure 1)

applyEveryNth :: MyPhotoM ()
applyEveryNth = do
  opts <- getOpts
  case opts of
    Options {optEveryNth = Just n} -> do
      logInfo ("applyEveryNth, value of n: " ++ show n)
      imgs <- getImgs
      putImgs (everyNth n imgs)
    _ -> return ()

makeImgsPathsAbsoluteAndCheckExistence :: MyPhotoM ()
makeImgsPathsAbsoluteAndCheckExistence = do
  logDebug "makeImgsPathsAbsoluteAndCheckExistence"
  withImgsIO $ \imgs -> do
    mapM
      ( \img -> do
          exists <- doesFileExist img
          unless exists $ do
            IO.hPutStrLn IO.stderr $ "image not found: " ++ img
            exitWith (ExitFailure 1)
          makeAbsolute img
      )
      imgs

sortOnCreateDate :: MyPhotoM ()
sortOnCreateDate = do
  opts <- getOpts
  when (optSortOnCreateDate opts) $ do
    logDebug "sorting on create date"
    withImgsIO $ \imgs -> do
      imgs' <- sortByCreateDate (optVerbose opts) imgs
      when (imgs /= imgs' && imgs /= reverse imgs') $ do
        logWarnIO "WARN: sorting on date changed order"
      return imgs'

applyBreaking :: MyPhotoM ()
applyBreaking = do
  opts <- getOpts
  case optBreaking opts of
    Nothing -> return ()
    Just gapInSeconds | gapInSeconds < 1 -> return ()
    Just gapInSeconds -> do
      case optEveryNth opts of
        Just _ -> do
          logInfo "ignoring --breaking because --every-nth is specified"
        _ -> do
          logInfo ("breaking on time gap of " ++ show gapInSeconds ++ " seconds")
          imgs <- getImgs
          broken <- MTL.liftIO $ applyBreakingToImgs (optVerbose opts) gapInSeconds imgs
          putImgs broken
          logTimeSinceStart "after applyBreaking"

applyUnRAW = do
  guardByExtensions unrawExtensions $ do
    step "unraw" . withImgsIO $ unRAW def

applyUnTiff = do
  guardByExtensions untiffExtensions $ do
    step "untiff" $ do
      opts <- getOpts
      withImgsIO $ unTiff (optClean opts /= NoCleanup)

applyUnHeif = do
  guardByExtensions unHeifExtensions $ do
    step "unheif" $ do
      opts <- getOpts
      withImgsIO $ unHeif (optClean opts /= NoCleanup)

applyRemoveOutliers :: MyPhotoM ()
applyRemoveOutliers = do
  wd <- getWdAndMaybeMoveImgs
  imgs <- getImgs
  metadatas <- MTL.liftIO $ getMetadataFromImgs False imgs
  let anyFlashWasFired = any (\(Metadata {_flashFired = flashFired}) -> flashFired) metadatas
  if anyFlashWasFired
    then step "remove outliers" . withImgsIO $ rmOutliers wd
    else logInfo "skipping outlier removal because no flash was fired in any image"

applyDownscale :: MyPhotoM ()
applyDownscale = do
  opts <- getOpts
  let downscalePct = optDownscalePct opts
  when (downscalePct /= 100) $ do
    step "downscale" $ do
      imgs <- getImgs
      downscaledImgs <- MTL.liftIO $ downscaleImgs (optDownscalePct opts) imgs
      putImgs downscaledImgs

createShellScript :: MyPhotoM ()
createShellScript = do
  outputBN <- getStackOutputBNFromImgs
  script <- MTL.liftIO $ makeAbsolute (outputBN ++ ".sh")
  scriptAlreadyExists <- MTL.liftIO $ doesFileExist script
  if scriptAlreadyExists
    then do
      logInfo ("shell script " ++ script ++ " already exists, not overwriting")
    else do
      wd <- getWdAndMaybeMoveImgs
      opts <- getOpts
      logInfo ("creating shell script at " ++ script)
      imgs <- getImgs
      let imgsRelativeToWd = map (makeRelative wd) imgs
      MTL.liftIO $ do
        let cmd = "myphoto-stack"
            exportOptArg =
              case optExport opts of
                NoExport -> ""
                Export -> "--export"
                ExportToParent -> "--export-to-parent"
            fullCmd = unwords [cmd, "--workdir \"$(pwd)\" --no-remove-outliers --no-breaking --no-sort", exportOptArg, "\"$@\""]
            scriptContent =
              unlines $
                [ "#!/usr/bin/env bash",
                  "set -euo pipefail",
                  "imgs=(" ++ (unwords (map (\img -> "\"" ++ img ++ "\"") imgsRelativeToWd)) ++ ")",
                  "cd \"$(dirname \"$0\")\"",
                  "echo -- '$ " ++ fullCmd ++ " [img [img [...]]]'",
                  "exec " ++ fullCmd ++ " \"${imgs[@]}\""
                ]
        IO.writeFile script scriptContent
        Ex.catch
          ( do
              perms <- getPermissions script
              setPermissions script (perms {executable = True})
          )
          ( \(_ :: Ex.IOException) -> do
              logWarnIO ("could not set executable permission on " ++ script)
          )

createMontage :: MyPhotoM ()
createMontage = step "montage" $ do
  outputBN <- getStackOutputBNFromImgs
  wd <- getWdAndMaybeMoveImgs
  imgs <- getImgs
  montageOut <- MTL.liftIO $ montageSample 16 200 (inWorkdir wd (outputBN <.> "all")) imgs

  opts <- getOpts
  when (optWorkdirStrategy opts == MoveExistingImgsToSubfolder) $ do
    _ <- MTL.liftIO $ reverseLink (wd </> "..") [montageOut]
    return ()

runFocusStack :: MyPhotoM [FilePath]
runFocusStack = step "focus stacking (and aligning) with PetteriAimonen/focus-stack" $ do
  imgs <- getImgs
  opts <- getOpts
  let additionalParameters = Map.findWithDefault [] "focus-stack" (optParameters opts)
  (focusStacked, aligned) <- MTL.liftIO $ focusStackImgs (optVerbose opts) (optNoGpu opts) additionalParameters imgs
  addOut focusStacked
  -- #if 0
  --         focusStackedAlignedOut <- MTL.liftIO $ montageSample 25 200 (focusStacked -<.> ".aligned") aligned
  -- #endif
  return aligned

runHuginAlign :: MyPhotoM [FilePath]
runHuginAlign = step "just aligning with hugin" $ do
  imgs <- getImgs
  opts <- getOpts
  wd <- getWdAndMaybeMoveImgs
  aligned <- MTL.liftIO $ align (AlignOptions (optVerbose opts) AlignNamingStrategySequential False True (optNoGpu opts)) wd imgs
  return aligned

runEnfuse :: [FilePath] -> MyPhotoM ()
runEnfuse aligned = step "focus stacking with enfuse" $ do
  outputBN <- getStackOutputBNFromImgs
  enfuseResult <- do
    opts <- getOpts
    MTL.liftIO $
      enfuseStackImgs
        ( def
            { eeOptions = def {eeVerbose = optVerbose opts},
              eeaOutputBN = Just outputBN,
              eeaChunk = optEnfuseChunkSettings opts
            }
        )
        aligned
  case enfuseResult of
    Left err -> fail err
    Right enfuseOuts -> do
      mapM_ earlyExportLayersTiff enfuseOuts
      addOuts enfuseOuts

runZereneStacker :: Bool -> [FilePath] -> MyPhotoM ()
runZereneStacker align imgs = step "focus stacking with Zerene Stacker" $ do
  outputBN <- getStackOutputBNFromImgs
  opts <- getOpts
  let chunkSettings =
        if align && optZereneStackerChunkSettings opts /= NoChunks
          then NoChunks
          else optZereneStackerChunkSettings opts
  when (align && optZereneStackerChunkSettings opts /= NoChunks) $
    logWarn "Zerene Stacker chunking disabled: images are not pre-aligned"
  let zsMode =
        case (optZereneStackerHeadless opts, optZereneStackerParallel opts, optZereneStackerChunkSettings opts) of
          (_, True, NoChunks) -> ZereneStackerParallel
          (True, _, NoChunks) -> ZereneStackerHeadless align
          (_, _, NoChunks) -> ZereneStackerDefault align
          (_, _, chunkSettings) -> ZereneStackerChunked chunkSettings
  zereneStackerResult <- MTL.liftIO $ zereneStacker (optVerbose opts) zsMode outputBN imgs
  case zereneStackerResult of
    Left err -> fail err
    Right zereneStackerOuts -> do
      mapM_ earlyExportLayersTiff zereneStackerOuts
      addOuts zereneStackerOuts

getExportTarget :: MyPhotoM (Maybe FilePath)
getExportTarget = do
  opts <- getOpts
  wd <- getWdOrFail
  case optExport opts of
    NoExport -> return Nothing
    Export -> return (Just "../0_stacked")
    ExportToParent -> return (Just (takeDirectory wd))

earlyExport :: FilePath -> MyPhotoM ()
earlyExport filePath = do
  target <- getExportTarget
  case target of
    Nothing -> return ()
    Just dir -> do
      logInfo $ "early exporting: " ++ filePath
      _ <- MTL.liftIO $ reverseLink dir [filePath]
      return ()

earlyExportLayersTiff :: FilePath -> MyPhotoM ()
earlyExportLayersTiff outFile = do
  let layersTiff = outFile -<.> "layers.tif"
  exists <- MTL.liftIO $ doesFileExist layersTiff
  when exists $ earlyExport layersTiff

maybeExport :: MyPhotoM ()
maybeExport = do
  target <- getExportTarget
  case target of
    Nothing -> return ()
    Just dir -> do
      logInfo "exporting images"
      outs <- getOuts
      if null outs
        then do
          logWarn "no outputs to export"
          return ()
        else do
          logInfo $ "Export to " ++ dir
          _ <- MTL.liftIO $ reverseLink dir outs
          return ()

alignOuts :: MyPhotoM ()
alignOuts = step "align outputs" $ do
  logWarn "Aligning outputs is currently flaky, might crop to much or might reduce quality"
  outs <- getOuts
  when (length outs >= 2) $ do
    wd <- getWdOrFail
    opts <- getOpts
    withOutsReplaceIO $ \outs -> do
      if null outs
        then do
          logInfoIO "no outputs to align"
          return outs
        else do
          aligned <- alignSmallerOnTopOfBiggest wd outs
          case optCropToCommonIntersectionFuzz opts of
            Nothing -> return aligned
            Just fuzz -> cropToCommonIntersection fuzz wd aligned

saveOutsAsMultilayerTiff :: FilePath -> Imgs -> IO FilePath
saveOutsAsMultilayerTiff outputTiff imgs = do
  let args = imgs ++ ["-compress", "LZW", outputTiff]
  logInfoIO $ "creating multilayer TIFF: " ++ outputTiff
  (_, _, _, pHandle) <- createProcess (proc "magick" args)
  exitCode <- waitForProcess pHandle
  unless (exitCode == ExitSuccess) $
    fail ("creating multilayer TIFF failed with " ++ show exitCode)
  return outputTiff

createMultilayerTiff :: MyPhotoM ()
createMultilayerTiff = step "create multilayer TIFF" $ do
  outs <- getOuts
  when (length outs >= 2) $ do
    wd <- getWdOrFail
    outputBN <- getStackOutputBNFromImgs
    let outputTiff = inWorkdir wd (outputBN <.> "layers.tif")
    _ <- MTL.liftIO $ saveOutsAsMultilayerTiff outputTiff outs
    earlyExport outputTiff

makeOutsPathsAbsolute :: MyPhotoM ()
makeOutsPathsAbsolute = do
  logDebug "makeOutsPathsAbsolute"
  withOutsReplaceIO $ \outs -> do
    mapM
      ( \out -> do
          exists <- doesFileExist out
          unless exists $ do
            IO.hPutStrLn IO.stderr $ "output not found: " ++ out
            exitWith (ExitFailure 1)
          makeAbsolute out
      )
      outs

maybeClean :: MyPhotoM ()
maybeClean = do
  opts <- getOpts
  wd <- getWdOrFail
  let cleanupStrategy = optClean opts
  case cleanupStrategy of
    NoCleanup -> return ()
    RemoveWorkdirRecursively -> do
      let exportStrategy = optExport opts
      when (exportStrategy == NoExport) $ do
        fail "cannot clean workdir when not exporting"
      logInfo $ "cleaning up work directory " ++ wd
      MTL.liftIO $ removeDirectoryRecursive wd

mkStage :: MyPhotoM a -> MyPhotoState -> IO (a, MyPhotoState)
mkStage stage startState =
  let stageWithLog = do
        logDebug ("mkStage start: " ++ show startState)
        a <- stage
        endState <- MTL.get
        logDebug ("mkStage end: " ++ show endState)
        return a
   in do
        (a, endState) <- MTL.runStateT stageWithLog startState
        print endState
        return (a, endState)

withinCurrentWorkdir :: MyPhotoM a -> MyPhotoM a
withinCurrentWorkdir action = do
  wd <- getWdAndMaybeMoveImgs
  MTL.liftIO $ do
    setCurrentDirectory wd
    logInfoIO ("work directory: " ++ wd)
  openLogFileForImgs
  action

runImportStage :: MyPhotoState -> IO (FilePath, MyPhotoState)
runImportStage =
  mkStage
    ( do
        readDirectoryIfOnlyOneWasSpecified
        failIfNoImagesWereSpecified
        makeImgsPathsAbsoluteAndCheckExistence
        sortOnCreateDate
        applyEveryNth
        withinCurrentWorkdir $ do
          guardWithOpts
            ( \opts ->
                let breaking = optBreaking opts
                 in isJust breaking && breaking > Just 0
            )
            applyBreaking
          applyUnRAW
          guardWithOpts optUnHeif applyUnHeif
          guardWithOpts optUntiff applyUnTiff
          guardWithOpts optRemoveOutliers applyRemoveOutliers
          -- createMontage
          createShellScript
          guardWithOpts ((/= 100) . optDownscalePct) $ do
            applyDownscale
        -- createShellScript

        getWdAndMaybeMoveImgs
    )

runStackStage :: MyPhotoState -> IO ((), MyPhotoState)
runStackStage =
  mkStage
    ( withinCurrentWorkdir $ do
        guardWithOpts
          ( \opts ->
              (optFocusStack opts || optEnfuse opts || optZereneStacker opts)
          )
          $ do
            opts <- getOpts
            if (optFocusStack opts || optEnfuse opts)
              then do
                -- Produce the image list that feeds enfuse / Zerene. If
                -- focus-stack runs, its aligned frames are the byproduct we
                -- want (unless alignment is disabled); otherwise we either
                -- align with hugin or pass the originals through.
                downstreamImgs <-
                  if optFocusStack opts
                    then do
                      focusStackAligned <- runFocusStack
                      if optAlign opts
                        then return focusStackAligned
                        else do
                          logWarn "discarding focus-stack aligned frames because --no-align was requested"
                          getImgs
                    else
                      if optAlign opts
                        then runHuginAlign
                        else getImgs
                guardWithOpts optEnfuse $ runEnfuse downstreamImgs
                guardWithOpts optZereneStacker $ runZereneStacker False downstreamImgs
              else do
                guardWithOpts optZereneStacker $ getImgs >>= runZereneStacker (optAlign opts)

        guardWithOpts optAlignOutputs alignOuts
        createMultilayerTiff
        maybeExport
        makeOutsPathsAbsolute
        maybeClean
        logTimeSinceStart "finished stateFun"
        closeLogFile
    )
