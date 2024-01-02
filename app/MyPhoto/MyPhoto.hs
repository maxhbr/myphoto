module MyPhoto.MyPhoto
  ( runMyPhoto
  )
where

import Control.Concurrent (getNumCapabilities)
import Control.Monad (unless, when)
import qualified Control.Monad.State.Lazy as MTL
import MyPhoto.Actions.Align
import MyPhoto.Actions.Enfuse
import MyPhoto.Actions.Exiftool
import MyPhoto.Actions.FocusStack
import MyPhoto.Actions.Montage
import MyPhoto.Actions.FileSystem
import MyPhoto.Actions.Outliers
import MyPhoto.Model
import System.Console.GetOpt
import System.Environment (getArgs, getProgName, withArgs)
import qualified System.IO as IO

startOptions :: Options
startOptions =
  Options
    { optVerbose = False,
      optCopy = False,
      optMove = False,
      optWorkdir = Nothing,
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
          (\arg opt -> return opt {optWorkdir = Just arg})
          "DIR"
      )
      "work directory",
    Option "" ["copy"] (NoArg (\opt -> return opt {optCopy = True})) "copy files to subfolder",
    Option "" ["move"] (NoArg (\opt -> return opt {optMove = True})) "move files to subfolder",
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
sampleOfM m xs = let
    len = length xs
    n = len `div` m
  in if len <= m
     then xs
     else everyNth n xs

getOptionsAndInitialize :: IO (Options, [FilePath])
getOptionsAndInitialize =
  let fillWorkDir :: [FilePath] -> Options -> IO Options
      fillWorkDir imgs opts@Options {optWorkdir = Just wd} = do
        absWd <- makeAbsolute wd
        return opts {optWorkdir = Just absWd}
      fillWorkDir imgs@(img : _) opts = let
            wd = if (optCopy opts || optMove opts)
                 then (takeDirectory img) </> "myphoto"
                 else let imgBN = computeStackOutputBN imgs
                       in (takeDirectory img) <.> imgBN <.> "myphoto"
        in return opts { optWorkdir = Just wd }
      fillWorkDir _ _ = undefined -- should not happen


      applySparse :: Options -> [FilePath] -> [FilePath]
      applySparse Options {optEveryNth = Just n} imgs = everyNth n imgs
      applySparse _ imgs = imgs
   in do
        args <- getArgs
        let (actions, imgs'', errors) = getOpt RequireOrder options args
        unless (null errors) $ do
          mapM_ (IO.hPutStrLn IO.stderr) errors
          exitWith (ExitFailure 1)

        imgs' <- case imgs'' of
          [maybeDir] -> do
            exists <- doesDirectoryExist maybeDir
            if exists
              then do
                IO.hPutStrLn IO.stderr ("directory specified: " ++ maybeDir)
                map (maybeDir </>) <$> listDirectory maybeDir
              else return imgs''
          _ -> return imgs''

        opts' <- foldl (>>=) (return startOptions) actions

        when (optCopy opts' && optMove opts') $ do
          IO.hPutStrLn IO.stderr ("cannot specify both --copy and --move")
          exitWith (ExitFailure 1)

        when (null imgs') $ do
          IO.hPutStrLn IO.stderr ("no image specified")
          exitWith (ExitFailure 1)

        imgs <-
          mapM
            ( \img -> do
                exists <- doesFileExist img
                unless exists $ do
                  IO.hPutStrLn IO.stderr $ "image not found: " ++ img
                  exitWith (ExitFailure 1)
                makeAbsolute img
            )
            (applySparse opts' imgs')

        opts <- fillWorkDir imgs opts'

        imgsMoved <- case opts of 
            Options {optWorkdir = Just wd, optMove = True} -> do
              let indir = wd </> "raw"
              IO.hPutStrLn IO.stderr ("INFO: move files to subfolder: " ++ indir)
              move indir imgs
            Options {optWorkdir = Just wd, optCopy = True} -> do
              let indir = wd </> "raw"
              IO.hPutStrLn IO.stderr ("INFO: copy files to subfolder: " ++ indir)
              copy indir imgs
            _ -> return imgs

        return (opts, imgsMoved)

myPhotoStateAction :: Options -> MTL.StateT (Imgs, Imgs) IO ()
myPhotoStateAction opts@Options {optVerbose = verbose} = do
  do
    MTL.liftIO $ IO.hPutStrLn IO.stderr ("INFO: create montage")
    imgs <- MTL.gets (sampleOfM 25 . fst)
    montageOut <- MTL.liftIO $ montage 100 (inWorkdir opts "all") imgs
    MTL.liftIO $ IO.hPutStrLn IO.stderr ("INFO: wrote " ++ montageOut)

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
    withoutOutliers <- MTL.liftIO $ rmOutliers (optWorkdir opts) imgs
    MTL.put (withoutOutliers, outs)

  outputBN <- do
    imgs <- MTL.gets fst
    return (computeStackOutputBN imgs)

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
      aligned <- MTL.liftIO $ align opts imgs
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
    montageOut <- MTL.liftIO $ montage 1000 (inWorkdir opts outputBN) outs
    MTL.liftIO $ IO.hPutStrLn IO.stderr ("INFO: wrote " ++ montageOut)

  return ()

runMyPhoto :: IO ()
runMyPhoto = do
  (opts@Options {optVerbose = verbose}, imgs) <- getOptionsAndInitialize
  setCurrentWD opts
  when verbose $ print opts

  (ret, (imgs', outs)) <- MTL.runStateT (myPhotoStateAction opts) (imgs, [])

  IO.hPutStrLn IO.stderr ("Done")
