{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MyPhoto.Impl
  ( runImportStage,
    runStackStage,
    computeRawImportDirInWorkdir,
  )
where

import Control.Concurrent (getNumCapabilities)
import qualified Control.Exception as Ex
import qualified Control.Monad.State.Lazy as MTL
import Data.List (isInfixOf)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Maybe (isJust)
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
import MyPhoto.Model
import MyPhoto.Monad
import MyPhoto.Video
import System.Console.GetOpt
import System.Directory (removeDirectoryRecursive)
import System.Environment (getArgs, getProgName, withArgs)
import qualified System.IO as IO

computeRawImportDirInWorkdir :: FilePath -> Imgs -> FilePath
computeRawImportDirInWorkdir wd imgs = wd </> computeStackOutputBN imgs <.> "raw"

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
                  let indir = computeRawImportDirInWorkdir absWd imgs
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
                imgs@(img0 : _) <- getImgs
                let imgBN = computeStackOutputBN imgs
                let wd = takeDirectory img0 <.> imgBN <.> "myphoto"
                MTL.liftIO $ createDirectoryIfMissing True wd
                return wd
              MoveExistingImgsToSubfolder -> do
                opts <- getOpts
                when (isJust (optEveryNth opts)) $ do
                  fail "cannot move images to subfolder when --every-nth is specified"
                imgs@(img0 : _) <- getImgs
                let wd = takeDirectory img0
                let imgBN = computeStackOutputBN imgs
                let indir = wd </> imgBN <.> "raw"
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
                imgs <- getImgs
                let wdWithSubdir = wd </> computeStackOutputBN imgs <.> "myphoto"
                implForImportToWorkdir wdWithSubdir
            setWd wd
            return wd

getOutputBN :: MyPhotoM FilePath
getOutputBN = computeStackOutputBN <$> getImgs

maybeRedirectLogToLogFile :: MyPhotoM a -> MyPhotoM a
maybeRedirectLogToLogFile action = do
  opts <- getOpts
  if optRedirectLog opts
    then redirectLogToLogFile action
    else action

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
          broken <- MTL.liftIO $ breaking (optVerbose opts) gapInSeconds imgs
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
  imgs <- getImgs
  script <- MTL.liftIO $ makeAbsolute (computeStackOutputBN imgs ++ ".sh")
  scriptAlreadyExists <- MTL.liftIO $ doesFileExist script
  if scriptAlreadyExists
    then do
      logInfo ("shell script " ++ script ++ " already exists, not overwriting")
    else do
      wd <- getWdAndMaybeMoveImgs
      opts <- getOpts
      logInfo ("creating shell script at " ++ script)
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

createMontage :: MyPhotoM ()
createMontage = step "montage" $ do
  outputBN <- getOutputBN
  imgs <- getImgs
  wd <- getWdAndMaybeMoveImgs
  montageOut <- MTL.liftIO $ montageSample 25 200 (inWorkdir wd (outputBN <.> "all")) imgs

  opts <- getOpts
  when (optWorkdirStrategy opts == MoveExistingImgsToSubfolder) $ do
    _ <- MTL.liftIO $ reverseLink (wd </> "..") [montageOut]
    return ()

runFocusStack :: MyPhotoM [FilePath]
runFocusStack = step "focus stacking (and aligning) with PetteriAimonen/focus-stack" $ do
  imgs <- getImgs
  opts <- getOpts
  let additionalParameters = Map.findWithDefault [] "focus-stack" (optParameters opts)
  (focusStacked, aligned) <- MTL.liftIO $ focusStackImgs (optVerbose opts) additionalParameters imgs
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
  aligned <- MTL.liftIO $ align (AlignOptions (optVerbose opts) AlignNamingStrategySequential False) wd imgs
  return aligned

runEnfuse :: [FilePath] -> MyPhotoM ()
runEnfuse aligned = step "focus stacking with enfuse" $ do
  outputBn <- getOutputBN
  enfuseResult <- do
    opts <- getOpts
    MTL.liftIO $
      enfuseStackImgs
        ( def
            { eeOptions = def {eeVerbose = optVerbose opts},
              eeaOutputBN = Just outputBn,
              eeaChunk = optEnfuseChunkSettings opts
            }
        )
        aligned
  case enfuseResult of
    Left err -> fail err
    Right enfuseOuts -> addOuts enfuseOuts

maybeExport :: MyPhotoM ()
maybeExport = do
  opts <- getOpts
  wd <- getWdOrFail
  let exportStrategy = optExport opts
  when (exportStrategy /= NoExport) $ do
    logInfo "exporting images"
    outs <- getOuts
    if (null outs)
      then do
        logWarn "no outputs to export"
        return ()
      else do
        let outputDir = "../0_stacked"
        case exportStrategy of
          Export -> do
            logInfo $ "Export to " ++ outputDir
            _ <- MTL.liftIO $ reverseLink outputDir outs
            return ()
          ExportToParent -> do
            logInfo $ "Export to parent directory of " ++ wd
            _ <- MTL.liftIO $ do
              let parentDir = takeDirectory wd
              MTL.liftIO $ reverseLink parentDir outs
            return ()
    return ()
  return ()

alignOuts :: MyPhotoM ()
alignOuts = step "align outputs" $ do
  outs <- getOuts
  when (length outs >= 2) $ do
    opts <- getOpts
    wd <- getWdOrFail
    withOutsReplaceIO $ \outs -> do
      if null outs
        then do
          logInfoIO "no outputs to align"
          return outs
        else
          align (AlignOptions (optVerbose opts) AlignNamingStrategyOriginal True) wd outs

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
mkStage stage startState = do
  (a, endState) <- MTL.runStateT stage startState
  print endState
  return (a, endState)

runImportStage :: MyPhotoState -> IO (FilePath, MyPhotoState)
runImportStage =
  mkStage
    ( do
        readDirectoryIfOnlyOneWasSpecified
        failIfNoImagesWereSpecified
        makeImgsPathsAbsoluteAndCheckExistence
        sortOnCreateDate
        applyEveryNth
        wd <- getWdAndMaybeMoveImgs
        maybeRedirectLogToLogFile $ do
          MTL.liftIO $ do
            setCurrentDirectory wd
            logInfoIO ("work directory: " ++ wd)
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
          createMontage
          createShellScript
          guardWithOpts ((/= 100) . optDownscalePct) $ do
            applyDownscale
        -- createShellScript

        getWdAndMaybeMoveImgs
    )

runStackStage :: MyPhotoState -> IO ((), MyPhotoState)
runStackStage =
  mkStage
    ( do
        guardWithOpts
          ( \opts ->
              (optFocusStack opts || optEnfuse opts)
          )
          $ do
            opts <- getOpts
            aligned <- do
              if optFocusStack opts
                then do
                  if optFocusStackToHuginFallback opts
                    then do
                      currentState <- MTL.get
                      result <- MTL.liftIO $ Ex.try $ MTL.runStateT runFocusStack currentState
                      case result of
                        Right (aligned, newState) -> do
                          MTL.put newState
                          return aligned
                        Left (ex :: Ex.SomeException) -> do
                          logWarn $ "focus-stack failed with exception: " ++ show ex
                          let veryHighPxParameters = ["--threads=6", "--focus-stack-parameter=--batchsize=3"]
                              addVeryHighPxParametersToMap = Map.insertWith (++) "focus-stack" veryHighPxParameters
                              modifyOpts opts = opts {optParameters = addVeryHighPxParametersToMap (optParameters opts)}
                              modifyStateWithModifiedOpts s = s {myPhotoStateOpts = modifyOpts (myPhotoStateOpts s)}
                          result' <- MTL.liftIO $ Ex.try $ MTL.runStateT runFocusStack (modifyStateWithModifiedOpts currentState)
                          case result' of
                            Right (aligned, newState) -> do
                              MTL.put newState
                              return aligned
                            Left (ex' :: Ex.SomeException) -> do
                              logWarn $ "focus-stack failed with exception: " ++ show ex'
                              logWarn "falling back to hugin alignment"
                              runHuginAlign
                    else runFocusStack
                else runHuginAlign
            guardWithOpts optEnfuse $ runEnfuse aligned
        alignOuts
        maybeExport
        makeOutsPathsAbsolute
        maybeClean
        logTimeSinceStart "finished stateFun"
    )
