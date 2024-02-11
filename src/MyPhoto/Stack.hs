{-# LANGUAGE CPP #-}

module MyPhoto.Stack
  ( runMyPhotoStack,
    runMyPhotoStack',
    runMyPhotoStack'',
    computeRawImportDirInWorkdir,
  )
where

import Control.Concurrent (getNumCapabilities)
import qualified Control.Monad.State.Lazy as MTL
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import MyPhoto.Actions.Align
import MyPhoto.Actions.EnblendEnfuse
import MyPhoto.Actions.FileSystem
import MyPhoto.Actions.FocusStack
import MyPhoto.Actions.Metadata
import MyPhoto.Actions.Montage
import MyPhoto.Actions.Outliers
import MyPhoto.Actions.UnRAW
import MyPhoto.Actions.UnTiff
import MyPhoto.Actions.UnHeif
import MyPhoto.Model
import MyPhoto.Monad
import MyPhoto.Video
import System.Console.GetOpt
import System.Environment (getArgs, getProgName, withArgs)
import qualified System.IO as IO

options :: [OptDescr (Options -> IO Options)]
options =
  [ Option
      "w"
      ["workdir"]
      ( ReqArg
          (\arg opt -> return opt {optWorkdirStrategy = WorkdirStrategyOverwrite arg})
          "DIR"
      )
      "overwrite work directory",
    Option
      ""
      ["replace", "use-input-dir-with-move"]
      (NoArg (\opt -> return opt {optWorkdirStrategy = MoveExistingImgsToSubfolder}))
      "use input directory as work directory",
    Option
      ""
      ["inplace", "use-input-dir-without-move"]
      (NoArg (\opt -> return opt {optWorkdirStrategy = NextToImgFiles}))
      "work in input directory",
    Option
      ""
      ["import", "import-to-workdir"]
      ( OptArg
          ( \arg opt -> do
              let wd = case arg of
                    Nothing -> "."
                    Just arg -> arg

              return opt {optWorkdirStrategy = ImportToWorkdir wd}
          )
          "DIR"
      )
      "import to work directory",
    Option
      ""
      ["every-nth", "sparse"]
      ( ReqArg
          (\arg opt -> return opt {optEveryNth = Just (read arg :: Int)})
          "N"
      )
      "just take every n-th image (at least first and last)",
    Option
      ""
      ["sort-on-create-date"]
      ( NoArg
          (\opt -> return opt {optSortOnCreateDate = True})
      )
      "Try to sort on create date",
    Option
      ""
      ["no-sort", "no-sort-on-create-date"]
      ( NoArg
          (\opt -> return opt {optSortOnCreateDate = False})
      )
      "Do not sort on create date",
    Option
      ""
      ["remove-outliers"]
      ( NoArg
          (\opt -> return opt {optRemoveOutliers = True})
      )
      "Remove outliers (default)",
    Option
      ""
      ["no-remove-outliers"]
      ( NoArg
          (\opt -> return opt {optRemoveOutliers = False})
      )
      "Do not remove outliers",
    Option
      ""
      ["breaking"]
      ( ReqArg
          ( \arg opt ->
              let intArg = read arg :: Int
               in return $
                    if intArg <= 0
                      then opt {optBreaking = Nothing}
                      else opt {optBreaking = Just intArg}
          )
          "SECONDS"
      )
      "break on time gap (0 to disable)",
    Option
      ""
      ["no-breaking"]
      ( NoArg
          (\opt -> return opt {optBreaking = Nothing})
      )
      "disable breaking",
    -- untiff
    Option
      ""
      ["untiff"]
      ( NoArg
          (\opt -> return opt {optUntiff = True})
      )
      "Run untiff, to convert tiff to png",
    Option
      ""
      ["no-untiff"]
      ( NoArg
          (\opt -> return opt {optUntiff = False})
      )
      "Do not run untiff (default)",
    -- untiff
    Option
     V""
      ["unheif"]
      ( NoArg
          (\opt -> return opt {optUnHeif = True})
      )
      "Run unheif, to convert HEIF to png",
    Option
      ""
      ["no-unheif"]
      ( NoArg
          (\opt -> return opt {optUnHeif = False})
      )
      "Do not run unheif (default)",
    -- focus stack
    Option
      ""
      ["no-focus-stack"]
      ( NoArg
          (\opt -> return opt {optFocusStack = False})
      )
      "Do not run focus stacking by PetteriAimonen/focus-stack",
    Option
      ""
      ["focus-stack-parameter"]
      ( ReqArg
          (\arg opt -> return opt {optParameters = Map.insertWith (++) "focus-stack" [arg] (optParameters opt)})
          "PARAMETER"
      )
      "Parameters for PetteriAimonen/focus-stack",
    -- Option
    --   ""
    --   ["focus-stack-batch-size"]
    --   ( ReqArg
    --       (\arg opt -> return opt {optFocusStackBatchSize = case read arg :: Int of
    --                                                           0 -> NoChunks
    --                                                           1 -> NoChunks
    --                                                           n -> ChunkSize n})
    --       "N"
    --   ),
    -- enfuse
    Option
      ""
      ["no-enfuse"]
      ( NoArg
          (\opt -> return opt {optEnfuse = False})
      )
      "Do not run enfuse",
    Option
      ""
      ["enfuse"]
      ( NoArg
          (\opt -> return opt {optEnfuse = True})
      )
      "Run enfuse (default)",
    Option
      ""
      ["enfuse-chunk-size"]
      ( ReqArg
          (\arg opt -> return opt {optEnfuseChunkSettings = case read arg :: Int of
                                                              0 -> NoChunks
                                                              1 -> NoChunks
                                                              n -> ChunkSize n})
          "N"
      )
      "Chunk size for enfuse",
    Option
      ""
      ["enfuse-all-variants"]
      ( NoArg
          (\opt -> return opt {optEnfuse = True})
      )
      "Run enfuse (default)",
    -- Option
    --   ""
    --   ["enfuse-parameter"]
    --   ( ReqArg
    --       (\arg opt -> return opt {optParameters = Map.insertWith (++) "enfuse" [arg] (optParameters opt)})
    --       "PARAMETER"
    --   )
    --   "Parameters for enfuse",
    -- help
    Option
      ""
      ["redirect-log-to-file"]
      ( NoArg
          (\opt -> return opt {optRedirectLog = True})
      )
      "Enable verbose messages",
    Option
      "v"
      ["verbose"]
      ( NoArg
          (\opt -> return opt {optVerbose = True})
      )
      "Enable verbose messages",
    Option
      "h"
      ["help"]
      ( NoArg
          ( \_ -> do
              prg <- getProgName
              IO.hPutStr IO.stderr (usageInfo prg options)
              IO.hPutStrLn IO.stderr "  IMG0 [IMG1]..."

              IO.hPutStrLn IO.stderr "  --dirs [ARG1[,ARG2[,...]] --] DIR1[,DIR2[,...]]"
              IO.hPutStrLn IO.stderr "  --video VID [ARG1[,ARG2[,...]]]"
              IO.hPutStrLn IO.stderr "  --high-mpx [ARG1[,ARG2[,...]]]"

              exitWith ExitSuccess
          )
      )
      "Show help"
  ]

computeRawImportDirInWorkdir :: FilePath -> Imgs -> FilePath
computeRawImportDirInWorkdir wd imgs = wd </> computeStackOutputBN imgs <.> "raw"

getWdAndMaybeMoveImgs :: MyPhotoM FilePath
getWdAndMaybeMoveImgs = do
  wd <- MTL.gets myPhotoStateWd
  case wd of
    Just wd -> return wd
    Nothing -> do
      logDebug ("determine working directory")
      Options {optWorkdirStrategy = workdirStrategy} <- getOpts
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
        ImportToWorkdir wd -> do
          Options {optEveryNth = everyNth} <- getOpts
          when (isJust everyNth) $ do
            fail "cannot import images to subfolder when --every-nth is specified"
          MTL.liftIO $ createDirectoryIfMissing True wd
          absWd <- MTL.liftIO $ makeAbsolute wd
          imgs <- getImgs
          let indir = computeRawImportDirInWorkdir absWd imgs
          logInfo ("copy images to " ++ indir)
          imgs' <- MTL.liftIO $ copy indir imgs
          putImgs imgs'
          return absWd
      setWd wd
      return wd

getOutputBN :: MyPhotoM FilePath
getOutputBN = computeStackOutputBN <$> getImgs

runMyPhotoStack'' :: Options -> [Options -> IO Options] -> [String] -> IO FilePath
runMyPhotoStack'' startOpts actions startImgs = do
  let readDirectoryIfOnlyOneWasSpecified :: MyPhotoM ()
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
      readActionsIntoOpts :: [Options -> IO Options] -> MyPhotoM ()
      readActionsIntoOpts actions = withOptsIO $ \opts -> foldl (>>=) (return opts) actions
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
            when (imgs /= imgs') $ do
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
      applyUnRAW = do
        guardByExtensions unrawExtensions $ do
          logInfo "run unraw"
          withImgsIO $ unRAW def
      applyUnTiff = do
        guardByExtensions untiffExtensions $ do
          logInfo "run untiff"
          withImgsIO $ unTiff False
      applyRemoveOutliers :: MyPhotoM ()
      applyRemoveOutliers = do
        logInfo "removing outliers"
        wd <- getWdAndMaybeMoveImgs
        withImgsIO $ rmOutliers wd
      createMontage :: MyPhotoM ()
      createMontage = do
        logInfo "create montage"
        outputBN <- getOutputBN
        imgs <- getImgs
        wd <- getWdAndMaybeMoveImgs
        montageOut <- MTL.liftIO $ montageSample 25 200 (inWorkdir wd (outputBN <.> "all")) imgs
        logInfo ("wrote " ++ montageOut)

        opts <- getOpts
        when (optWorkdirStrategy opts == MoveExistingImgsToSubfolder) $ do
          _ <- MTL.liftIO $ reverseLink (wd </> "..") [montageOut]
          return ()
      runFoucsStack :: MyPhotoM [FilePath]
      runFoucsStack = do
        logInfo "focus stacking (and aligning) with PetteriAimonen/focus-stack"
        imgs <- getImgs
        opts <- getOpts
        let additionalParameters = Map.findWithDefault [] "focus-stack" (optParameters opts)
        (focusStacked, aligned) <- MTL.liftIO $ focusStackImgs (optVerbose opts) additionalParameters imgs
        addOut focusStacked
        focusStackedAlignedOut <- MTL.liftIO $ montageSample 25 200 (focusStacked -<.> ".aligned") aligned
        return aligned

      runHuginAlign :: MyPhotoM [FilePath]
      runHuginAlign = do
        logInfo "just aligning with hugin"
        imgs <- getImgs
        opts <- getOpts
        wd <- getWdAndMaybeMoveImgs
        aligned <- MTL.liftIO $ align (optVerbose opts) wd imgs
        return aligned

      runEnfuse :: [FilePath] -> MyPhotoM ()
      runEnfuse aligned = do
        logInfo "focus stacking with enfuse"
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

      makeOutsPathsAbsolute :: MyPhotoM ()
      makeOutsPathsAbsolute = do
        logDebug "makeOutsPathsAbsolute"
        withOutsIO $ \outs -> do
          mapM
            ( \out -> do
                exists <- doesFileExist out
                unless exists $ do
                  IO.hPutStrLn IO.stderr $ "output not found: " ++ out
                  exitWith (ExitFailure 1)
                makeAbsolute out
            )
            outs
      maybeRedirectLogToLogFile :: MyPhotoM a -> MyPhotoM a
      maybeRedirectLogToLogFile action = do
        opts <- getOpts
        if optRedirectLog opts
          then redirectLogToLogFile action
          else action

  let stateFun = do
        readDirectoryIfOnlyOneWasSpecified
        readActionsIntoOpts actions
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
          aligned <- do
            opts <- getOpts
            if optFocusStack opts
              then runFoucsStack
              else runHuginAlign
          guardWithOpts optEnfuse $ runEnfuse aligned
          makeOutsPathsAbsolute

        getWdAndMaybeMoveImgs

  (wd, endState) <- MTL.runStateT stateFun (startMyPhotoState startOpts startImgs)
  print endState
  return wd

runMyPhotoStack' :: [String] -> IO ()
runMyPhotoStack' args = do
  let (actions, startImgs, errors) = getOpt RequireOrder options args
  unless (null errors) $ do
    mapM_ (IO.hPutStrLn IO.stderr) errors
    exitWith (ExitFailure 1)
  _ <- runMyPhotoStack'' def actions startImgs
  return ()

runMyPhotoStackForVideo :: FilePath -> [String] -> IO ()
runMyPhotoStackForVideo vid args = do
  isExistingFile <- doesFileExist vid
  unless isExistingFile $ do
    IO.hPutStrLn IO.stderr ("video not found: " ++ vid)
    exitWith (ExitFailure 1)
  imgs <- extractFrames vid
  let args' = args ++ ["--workdir", takeDirectory vid, "--no-remove-outliers", "--no-breaking", "--no-sort"] ++ imgs
  runMyPhotoStack' args'

runMyPhotoStackForDirs :: [FilePath] -> [String] -> IO ()
runMyPhotoStackForDirs dirs args = do
  mapM_
    ( \dir -> do
        isExistingDirectory <- doesDirectoryExist dir
        unless isExistingDirectory $ do
          IO.hPutStrLn IO.stderr ("directory not found: " ++ dir)
          exitWith (ExitFailure 1)
        runMyPhotoStack' (args ++ [dir])
    )
    dirs

runMyPhotoStack :: IO ()
runMyPhotoStack = do
  numCapabilities <- getNumCapabilities
  when (numCapabilities == 1) $
    IO.hPutStrLn IO.stderr ("WARN: numCapabilities=" ++ show numCapabilities ++ " this looks suspicious")

  args <- getArgs
  case args of
    "--video" : vid : args' -> do
      runMyPhotoStackForVideo vid args'
    "--dirs" : args' -> do
      let (args'', dirs) = case splitOn ["--"] args' of
            [args'', dirs] -> (args'', dirs)
            [dirs] -> ([], dirs)
            _ -> error "invalid args"
      runMyPhotoStackForDirs dirs args''
    "--high-mpx" : args' -> do
      runMyPhotoStack' (["--focus-stack-parameter=--batchsize=6", "--focus-stack-parameter=--threads=14"]++args')
    _ -> runMyPhotoStack' args
