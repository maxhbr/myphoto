module MyPhoto.Stack
  ( runMyPhotoStack,
  )
where

import Control.Concurrent (getNumCapabilities)
import qualified Control.Monad.State.Lazy as MTL
import Data.List.Split (splitOn)
import MyPhoto.Actions.Align
import MyPhoto.Actions.Enfuse
import MyPhoto.Actions.Exiftool
import MyPhoto.Actions.FileSystem
import MyPhoto.Actions.FocusStack
import MyPhoto.Actions.Montage
import MyPhoto.Actions.Outliers
import MyPhoto.Model
import MyPhoto.Video
import System.Console.GetOpt
import System.Environment (getArgs, getProgName, withArgs)
import qualified System.IO as IO

startOptions :: Options
startOptions =
  Options
    { optVerbose = False,
      optWorkdirStrategy = CreateNextToImgDir,
      optEveryNth = Nothing,
      optRemoveOutliers = True,
      optBreaking = Just 300,
      optEnfuse = True,
      optFocusStack = True
    }

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
      ["use-input-dir"]
      (NoArg (\opt -> return opt {optWorkdirStrategy = MoveExistingImgsToSubfolder}))
      "use input directory as work directory",
    Option
      ""
      ["use-input-dir-without-move"]
      (NoArg (\opt -> return opt {optWorkdirStrategy = NextToImgFiles}))
      "work in input directory",
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
      ["no-focus-stack"]
      ( NoArg
          (\opt -> return opt {optFocusStack = False})
      )
      "Do not run focus stacking by PetteriAimonen/focus-stack",
    Option
      ""
      ["every-nth"]
      ( ReqArg
          (\arg opt -> return opt {optEveryNth = Just (read arg :: Int)})
          "N"
      )
      "just take every n-th image (at least first and last)",
    Option
      ""
      ["no-remove-outliers"]
      ( NoArg
          (\opt -> return opt {optRemoveOutliers = False})
      )
      "Do not remove outliers",
    Option
      ""
      ["remove-outliers"]
      ( NoArg
          (\opt -> return opt {optRemoveOutliers = True})
      )
      "Remove outliers (default)",
    Option
      ""
      ["breaking"]
      ( ReqArg
          (\arg opt -> return opt {optBreaking = Just (read arg :: Int)})
          "SECONDS"
      )
      "break on time gap (0 to disable)",
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

              numCapabilities <- getNumCapabilities
              when (numCapabilities == 1) $
                IO.hPutStrLn IO.stderr ("WARN: numCapabilities=" ++ show numCapabilities ++ " this looks suspicious")

              exitWith ExitSuccess
          )
      )
      "Show help"
  ]

everyNth :: Int -> [a] -> [a]
everyNth _ [] = []
everyNth n (x : xs) =
  let everyNth' :: Int -> [a] -> [a]
      everyNth' n xs = case drop (n - 1) xs of
        (y : ys) -> y : everyNth n ys
        [] -> [last xs | not (null xs)]
   in x : everyNth' n xs

sampleOfM :: Int -> [a] -> [a]
sampleOfM _ [] = []
sampleOfM m xs =
  let len = length xs
      n = len `div` m
   in if len <= m
        then xs
        else everyNth n xs

getOptionsAndInitialize :: [String] -> MTL.StateT (Options, [FilePath]) IO ()
getOptionsAndInitialize =
  let getActionsFromArgs :: [String] -> MTL.StateT (Options, [FilePath]) IO [Options -> IO Options]
      getActionsFromArgs args = do
        (opts, imgs) <- MTL.get
        let (actions, imgs', errors) = getOpt RequireOrder options args
        MTL.put (opts, imgs ++ imgs')

        unless (null errors) $ MTL.liftIO $ do
          mapM_ (IO.hPutStrLn IO.stderr) errors
          exitWith (ExitFailure 1)

        return actions
      readDirectoryIfOnlyOneWasSpecified :: MTL.StateT (Options, [FilePath]) IO ()
      readDirectoryIfOnlyOneWasSpecified = do
        (opts, imgs) <- MTL.get
        imgs' <- case imgs of
          [maybeDir] -> MTL.liftIO $ do
            isExistingDirectory <- doesDirectoryExist maybeDir
            if isExistingDirectory
              then do
                IO.hPutStrLn IO.stderr ("directory specified: " ++ maybeDir)
                imgs' <- map (maybeDir </>) <$> listDirectory maybeDir
                return imgs'
              else return imgs
          _ -> return imgs
        MTL.put (opts, imgs')
      readActionsIntoOpts :: [Options -> IO Options] -> MTL.StateT (Options, [FilePath]) IO ()
      readActionsIntoOpts actions = do
        (opts, imgs) <- MTL.get
        opts' <- MTL.liftIO $ foldl (>>=) (return opts) actions
        MTL.put (opts', imgs)
      failIfNoImagesWereSpecified :: MTL.StateT (Options, [FilePath]) IO ()
      failIfNoImagesWereSpecified = do
        (opts, imgs) <- MTL.get
        when (null imgs) $ MTL.liftIO $ do
          IO.hPutStrLn IO.stderr ("no image specified")
          exitWith (ExitFailure 1)
      applyEveryNth :: MTL.StateT (Options, [FilePath]) IO ()
      applyEveryNth = do
        (opts, imgs) <- MTL.get
        case opts of
          Options {optEveryNth = Just n} -> do
            let imgs' = everyNth n imgs
            MTL.put (opts, imgs')
          _ -> return ()
      makeImgsPathsAbsoluteAndCheckExistence :: MTL.StateT (Options, [FilePath]) IO ()
      makeImgsPathsAbsoluteAndCheckExistence = do
        (opts, imgs) <- MTL.get
        imgs' <-
          MTL.liftIO $
            mapM
              ( \img -> do
                  exists <- doesFileExist img
                  unless exists $ do
                    IO.hPutStrLn IO.stderr $ "image not found: " ++ img
                    exitWith (ExitFailure 1)
                  makeAbsolute img
              )
              imgs
        MTL.put (opts, imgs')
   in \args -> do
        actions <- getActionsFromArgs args
        readDirectoryIfOnlyOneWasSpecified
        readActionsIntoOpts actions
        failIfNoImagesWereSpecified
        applyEveryNth
        makeImgsPathsAbsoluteAndCheckExistence

        return ()

myPhotoStateAction :: Options -> MTL.StateT (Imgs, Imgs) IO ()
myPhotoStateAction opts@Options {optVerbose = verbose} = do
  wd <- case optWorkdirStrategy opts of
    CreateNextToImgDir -> do
      imgs@(img0 : _) <- MTL.gets fst
      let imgBN = computeStackOutputBN imgs
      let wd = (takeDirectory img0) <.> imgBN <.> "myphoto"
      MTL.liftIO $ createDirectoryIfMissing True wd
      return wd
    MoveExistingImgsToSubfolder -> do
      (imgs@(img0 : _), outs) <- MTL.get
      let wd = takeDirectory img0
      let imgBN = computeStackOutputBN imgs
      let indir = wd </> imgBN <.> "raw"
      imgs' <- MTL.liftIO $ move indir imgs
      MTL.put (imgs', outs)
      return wd
    NextToImgFiles -> do
      (imgs@(img0 : _), outs) <- MTL.get
      let wd = takeDirectory img0
      return wd
    WorkdirStrategyOverwrite wd -> do
      MTL.liftIO $ createDirectoryIfMissing True wd
      absWd <- MTL.liftIO $ makeAbsolute wd
      return absWd
  MTL.liftIO $ do
    setCurrentDirectory wd
    IO.hPutStrLn IO.stderr ("INFO: work directory: " ++ wd)

  case optBreaking opts of
    Nothing -> return ()
    Just gapInSeconds | gapInSeconds < 1 -> return ()
    Just gapInSeconds -> do
      case (optEveryNth opts) of
        Just _ -> do
          MTL.liftIO $ IO.hPutStrLn IO.stderr ("INFO: ignoring --breaking because --every-nth is specified")
        _ -> do
          MTL.liftIO $ IO.hPutStrLn IO.stderr ("INFO: breaking on time gap of " ++ show gapInSeconds ++ " seconds")
          (imgs, outs) <- MTL.get
          broken <- MTL.liftIO $ breaking gapInSeconds imgs
          MTL.put (broken, outs)

  when (optRemoveOutliers opts) $ do
    MTL.liftIO $ IO.hPutStrLn IO.stderr ("INFO: removing outliers")
    (imgs, outs) <- MTL.get
    withoutOutliers <- MTL.liftIO $ rmOutliers wd imgs
    MTL.put (withoutOutliers, outs)

  outputBN <- do
    imgs <- MTL.gets fst
    return (computeStackOutputBN imgs)

  do
    MTL.liftIO $ IO.hPutStrLn IO.stderr ("INFO: create montage")
    imgs <- MTL.gets (sampleOfM 25 . fst)
    montageOut <- MTL.liftIO $ montage 100 (inWorkdir wd (outputBN <.> "all")) imgs
    MTL.liftIO $ IO.hPutStrLn IO.stderr ("INFO: wrote " ++ montageOut)
    when (optWorkdirStrategy opts == MoveExistingImgsToSubfolder) $ do
      MTL.liftIO $ reverseLink (wd </> "..") [montageOut]
      return ()

  if optFocusStack opts
    then do
      MTL.liftIO $ IO.hPutStrLn IO.stderr ("INFO: focus stacking (and aligning) with PetteriAimonen/focus-stack")
      (imgs, outs) <- MTL.get
      (focusStacked, aligned) <- MTL.liftIO $ focusStackImgs opts imgs
      MTL.put (aligned, outs ++ [focusStacked])
      return aligned
    else do
      MTL.liftIO $ IO.hPutStrLn IO.stderr ("INFO: just aligning with hugin")
      (imgs, outs) <- MTL.get
      aligned <- MTL.liftIO $ align wd opts imgs
      MTL.put (aligned, outs)
      return aligned

  when (optEnfuse opts) $ do
    MTL.liftIO $ IO.hPutStrLn IO.stderr ("INFO: focus stacking with enfuse")
    (imgs, outs) <- MTL.get
    enfuseResult <-
      MTL.liftIO $
        enfuseStackImgs
          ( enfuseDefaultOptions
              { optOutputBN = Just outputBN,
                optEnfuseVerbose = verbose
              }
          )
          imgs
    case enfuseResult of
      Left err -> MTL.liftIO $ IO.hPutStrLn IO.stderr err
      Right enfuseOuts -> MTL.put (imgs, outs ++ enfuseOuts)

  do
    MTL.liftIO $ IO.hPutStrLn IO.stderr ("INFO: create montage")
    outs <- MTL.gets snd
    montageOut <- MTL.liftIO $ montage 1000 (inWorkdir wd outputBN) outs
    MTL.liftIO $ IO.hPutStrLn IO.stderr ("INFO: wrote " ++ montageOut)

  return ()

runMyPhotoStack' :: IO ()
runMyPhotoStack' = do
  args <- getArgs
  (_, (opts, imgs)) <-
    (MTL.runStateT (getOptionsAndInitialize args) (startOptions, []))
  IO.hPutStrLn IO.stderr ("DEBUG: " ++ show opts)

  (_, (_, outs)) <- MTL.runStateT (myPhotoStateAction opts) (imgs, [])

  mapM_
    ( \out -> do
      IO.hPutStrLn IO.stderr ("INFO: output: " ++ out)
    )
    outs

  IO.hPutStrLn IO.stderr ("Done")

runMyPhotoStackForVideo :: FilePath -> [String] -> IO ()
runMyPhotoStackForVideo vid args = do
  isExistingFile <- doesFileExist vid
  unless isExistingFile $ do
    IO.hPutStrLn IO.stderr ("video not found: " ++ vid)
    exitWith (ExitFailure 1)
  imgs <- extractFrames vid
  let args' = args ++ ["--workdir", takeDirectory vid] ++ imgs
  withArgs args' runMyPhotoStack'


runMyPhotoStack :: IO ()
runMyPhotoStack = do
  args <- getArgs
  case args of
    "--video" : vid : args' -> do
      runMyPhotoStackForVideo vid args'
    "--dirs" : args' -> do
      let (args'', dirs) = case splitOn ["--"] args' of
            [args'', dirs] -> (args'', dirs)
            [dirs] -> ([], dirs)
            _ -> error "invalid args"
      mapM_
        ( \dir -> do
            isExistingDirectory <- doesDirectoryExist dir
            unless isExistingDirectory $ do
              IO.hPutStrLn IO.stderr ("directory not found: " ++ dir)
              exitWith (ExitFailure 1)
            withArgs (args'' ++ [dir]) runMyPhotoStack'
        )
        dirs
    _ -> runMyPhotoStack'
