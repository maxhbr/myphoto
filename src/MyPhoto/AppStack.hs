{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MyPhoto.AppStack
  ( runMyPhotoStack,
    runMyPhotoStack',
    runMyPhotoStack'',
    options,
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
import MyPhoto.Impl
import MyPhoto.Model
import MyPhoto.Monad
import MyPhoto.Video
import System.Console.GetOpt
import System.Directory (removeDirectoryRecursive)
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
      ["export"]
      ( NoArg
          (\opt -> return opt {optExport = Export})
      )
      "export to new work directory next to input directory",
    Option
      ""
      ["export-to-parent"]
      ( NoArg
          (\opt -> return opt {optExport = ExportToParent})
      )
      "export to parent directory",
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
      ""
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
    Option
      ""
      ["downscale"]
      ( ReqArg
          (\arg opt -> return opt {optDownscalePct = read arg :: Int})
          "PCT"
      )
      "Downscale images to PCT percent (default 100)",
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
      ["focus-stack"]
      ( NoArg
          (\opt -> return opt {optFocusStack = True})
      )
      "Run focus stacking by PetteriAimonen/focus-stack",
    Option
      ""
      ["focus-stack-parameter"]
      ( ReqArg
          (\arg opt -> return opt {optParameters = Map.insertWith (++) "focus-stack" [arg] (optParameters opt)})
          "PARAMETER"
      )
      "Parameters for PetteriAimonen/focus-stack",
    Option
      ""
      ["high-mpx"]
      ( NoArg
          (\opt -> return opt {optParameters = Map.insertWith (++) "focus-stack" ["--batchsize=6", "--threads=14"] (optParameters opt)})
      )
      "Enable high MPX mode",
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
          ( \arg opt ->
              return
                opt
                  { optEnfuseChunkSettings = case read arg :: Int of
                      0 -> NoChunks
                      1 -> NoChunks
                      n -> ChunkSize n
                  }
          )
          "N"
      )
      "Chunk size for enfuse",
    Option
      ""
      ["enfuse-sparse-chunk-size"]
      ( ReqArg
          ( \arg opt ->
              return
                opt
                  { optEnfuseChunkSettings = case read arg :: Int of
                      0 -> NoChunks
                      1 -> NoChunks
                      n -> SparseChunksOfSize n
                  }
          )
          "N"
      )
      "Chunk size for enfuse using sparse chunking",
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
    Option
      ""
      ["only-impoort"]
      ( NoArg
          (\opt -> return opt {optFocusStack = False, optEnfuse = False})
      )
      "only import images, no processing",
    Option
      ""
      ["clean-workdir-recursively"]
      ( NoArg
          (\opt -> return opt {optClean = RemoveWorkdirRecursively})
      )
      "Clean work directory recursively after processing",
    Option
      ""
      ["redirect-log-to-file"]
      ( NoArg
          (\opt -> return opt {optRedirectLog = True})
      )
      "Redirect log to log file in work directory",
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
              IO.hPutStrLn IO.stderr "  --import-dir DIR [ARG1[,ARG2[,...]]]"
              IO.hPutStrLn IO.stderr "  --video VID [ARG1[,ARG2[,...]]]"
              IO.hPutStrLn IO.stderr "  --only-stack [ARG1[,ARG2[,...]]]"

              exitWith ExitSuccess
          )
      )
      "Show help"
  ]

failIfDirDoesNotExist :: FilePath -> IO ()
failIfDirDoesNotExist dir = do
  isExistingDirectory <- doesDirectoryExist dir
  unless isExistingDirectory $ do
    IO.hPutStrLn IO.stderr ("ERR: directory not found: " ++ dir)
    exitWith (ExitFailure 1)

runMyPhotoStack'' :: Options -> [Options -> IO Options] -> [String] -> IO (FilePath, MyPhotoState)
runMyPhotoStack'' startOpts actions startImgs = do
  opts <- foldl (>>=) (return startOpts) actions
  startState <- startMyPhotoState opts startImgs
  (wd, importedState) <- runImportStage startState
  (_, endState) <- runStackStage importedState
  return (wd, endState)

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
        failIfDirDoesNotExist dir
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
    "--import-dir" : dir : args' -> do
      failIfDirDoesNotExist dir
      let basename = takeFileName (dropTrailingPathSeparator dir)
      runMyPhotoStack' (("--import=./0_raw_" ++ basename) : args' ++ [dir])
    _ -> runMyPhotoStack' args
