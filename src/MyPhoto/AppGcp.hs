{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module MyPhoto.AppGcp
  ( runMyPhotoGcp,
  )
where

import Control.Exception
import Control.Monad
import Data.Default
import Data.Maybe
import MyPhoto.Model
import MyPhoto.Actions.Gcp
import System.Console.GetOpt
import System.Directory (doesDirectoryExist, makeAbsolute)
import System.Environment (getArgs, getProgName)
import qualified System.IO as IO
import Data.Time.Clock
import Data.Time.Format

data GcpMode
  = RunMain
  | Teardown String (Maybe String)
  | DirectDownload String (Maybe FilePath)
  deriving (Show, Eq)

data GcpOptions = GcpOptions
  { optMode :: GcpMode,
    optInputDir :: Maybe FilePath,
    optOutputDir :: Maybe FilePath,
    optInputBucket :: Maybe String,
    optMachineType :: String,
    optDiskSize :: String,
    optImageFamily :: String,
    optImageProject :: String,
    optKeepVm :: Bool,
    optKeepBucket :: Bool,
    optDetach :: Bool
  }
  deriving (Show, Eq)

instance Default GcpOptions where
  def =
    GcpOptions
      { optMode = RunMain,
        optInputDir = Nothing,
        optOutputDir = Nothing,
        optInputBucket = Nothing,
        optMachineType = "n2-standard-32",
        optDiskSize = "500GB",
        optImageFamily = "debian-12",
        optImageProject = "debian-cloud",
        optKeepVm = False,
        optKeepBucket = False,
        optDetach = False
      }

gcpOptions :: [OptDescr (GcpOptions -> IO GcpOptions)]
gcpOptions =
  [ Option
      [] ["input-dir"]
      ( ReqArg
          ( \arg opt ->
              return opt {optInputDir = Just arg}
          )
          "DIR"
      )
      "Local input directory to upload",
    Option
      [] ["output-dir"]
      ( ReqArg
          ( \arg opt ->
              return opt {optOutputDir = Just arg}
          )
          "DIR"
      )
      "Local output directory for results",
    Option
      [] ["input-bucket"]
      ( ReqArg
          ( \arg opt ->
              return opt {optInputBucket = Just arg}
          )
          "BUCKET"
      )
      "Existing input bucket URI (gs://...)",
    Option
      [] ["machine-type"]
      ( ReqArg
          ( \arg opt ->
              return opt {optMachineType = arg}
          )
          "TYPE"
      )
      "Machine type (default: n2-standard-32)",
    Option
      [] ["disk-size"]
      ( ReqArg
          ( \arg opt ->
              return opt {optDiskSize = arg}
          )
          "SIZE"
      )
      "Boot disk size (default: 500GB)",
    Option
      [] ["image-family"]
      ( ReqArg
          ( \arg opt ->
              return opt {optImageFamily = arg}
          )
          "FAMILY"
      )
      "Image family (default: debian-12)",
    Option
      [] ["image-project"]
      ( ReqArg
          ( \arg opt ->
              return opt {optImageProject = arg}
          )
          "PROJECT"
      )
      "Image project (default: debian-cloud)",
    Option
      [] ["keep-vm"]
      ( NoArg
          ( \opt ->
              return opt {optKeepVm = True}
          )
      )
      "Keep VM after completion",
    Option
      [] ["keep-bucket"]
      ( NoArg
          ( \opt ->
              return opt {optKeepBucket = True}
          )
      )
      "Keep input bucket after completion",
    Option
      [] ["detach"]
      ( NoArg
          ( \opt ->
              return opt {optDetach = True}
          )
      )
      "Run in detached mode",
    Option
      "h" ["help"]
      ( NoArg
          ( \_ -> do
              prg <- getProgName
              IO.hPutStr IO.stderr (usageInfo prg gcpOptions)
              IO.hPutStrLn IO.stderr ""
              IO.hPutStrLn IO.stderr "Modes:"
              IO.hPutStrLn IO.stderr "  --teardown <vm-name> [<input-bucket>]"
              IO.hPutStrLn IO.stderr "  --direct-download <vm-name> [<output-dir>]"
              exitWith ExitSuccess
          )
      )
      "Show help"
  ]

readGcpConfig :: IO GcpConfig
readGcpConfig = do
  let envPath = "~/.myphoto/gcp.env"
  expandedPath <- catch (makeAbsolute envPath) (\(_ :: SomeException) -> return envPath)
  contents <- catch (readFile expandedPath) (\(_ :: SomeException) -> do
    IO.hPutStrLn IO.stderr ("GCP configuration file " ++ envPath ++ " not found.")
    exitWith (ExitFailure 1))
  let parseLine line =
        case words $ map (\c -> if c == '=' then ' ' else c) line of
          [key, value] -> Just (key, filter (not . (`elem` ['"', '\''])) value)
          _ -> Nothing
      parsed = mapMaybe parseLine (lines contents)
      getVar var = lookup var parsed
  gcpProject <- case getVar "PROJECT" of
    Just p -> return p
    Nothing -> do
      IO.hPutStrLn IO.stderr "Missing PROJECT in GCP configuration."
      exitWith (ExitFailure 1)
  gcpRegion <- case getVar "REGION" of
    Just r -> return r
    Nothing -> do
      IO.hPutStrLn IO.stderr "Missing REGION in GCP configuration."
      exitWith (ExitFailure 1)
  gcpZone <- case getVar "ZONE" of
    Just z -> return z
    Nothing -> do
      IO.hPutStrLn IO.stderr "Missing ZONE in GCP configuration."
      exitWith (ExitFailure 1)
  return GcpConfig {..}

generateVmName :: Maybe FilePath -> IO String
generateVmName maybeInputDir = do
  let size = case maybeInputDir of
        Just _ -> "local"
        Nothing -> "gs"
  now <- getCurrentTime
  let date = formatTime defaultTimeLocale "%Y%m%d-%H%M%S" now
  return $ "myphoto-" ++ date ++ "-" ++ size

runMyPhotoGcp :: IO ()
runMyPhotoGcp = do
  args <- getArgs
  
  let (actions, nonOpts, errors) = getOpt RequireOrder gcpOptions args
  unless (null errors) $ do
    mapM_ (IO.hPutStrLn IO.stderr) errors
    exitWith (ExitFailure 1)
  
  opts <- foldM (flip ($)) def actions
  
  case nonOpts of
    ["--teardown", vmName] -> do
      let opts' = opts {optMode = Teardown vmName Nothing}
      runWithOpts opts'
    ["--teardown", vmName, inputBucket] -> do
      let opts' = opts {optMode = Teardown vmName (Just inputBucket)}
      runWithOpts opts'
    ["--direct-download", vmName] -> do
      let opts' = opts {optMode = DirectDownload vmName (optOutputDir opts)}
      runWithOpts opts'
    ["--direct-download", vmName, outputDir] -> do
      let opts' = opts {optMode = DirectDownload vmName (Just outputDir)}
      runWithOpts opts'
    _ ->
      case optMode opts of
        RunMain -> runWithOpts opts
        _ -> do
          IO.hPutStrLn IO.stderr "Invalid arguments for mode."
          exitWith (ExitFailure 1)

runWithOpts :: GcpOptions -> IO ()
runWithOpts opts@GcpOptions {optMode = mode, optInputDir = inputDir, optOutputDir = outputDir} = do
  gcpConfig <- readGcpConfig
  
  case mode of
    RunMain -> do
      when (isNothing (optInputBucket opts) && isNothing inputDir) $ do
        IO.hPutStrLn IO.stderr "Either --input-bucket or --input-dir must be specified."
        exitWith (ExitFailure 1)
      when (isNothing outputDir) $ do
        IO.hPutStrLn IO.stderr "Missing --output-dir parameter."
        exitWith (ExitFailure 1)
      
      forM_ inputDir $ \dir -> do
        exists <- doesDirectoryExist dir
        unless exists $ do
          IO.hPutStrLn IO.stderr ("Input directory does not exist: " ++ dir)
          exitWith (ExitFailure 1)
      
      vmName <- generateVmName inputDir
      putStrLn ("VM name: " ++ vmName)
      
    Teardown vmName maybeInputBucket -> do
      putStrLn ("Tearing down VM: " ++ vmName)
      teardown vmName maybeInputBucket gcpConfig
      
    DirectDownload vmName maybeOutputDir -> do
      putStrLn ("Downloading from VM: " ++ vmName)
      directDownload vmName maybeOutputDir gcpConfig