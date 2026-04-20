{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module MyPhoto.Config
  ( loadOptionsConfig,
    loadJsonConfig,
    configDir,
    applyOptionsOverrides,
  )
where

-- Exported:
-- \* loadOptionsConfig   - Load Options from ~/.myphoto/options.json
-- \* loadJsonConfig      - Generic JSON config file loader
-- \* configDir           - Get ~/.myphoto path
-- \* applyOptionsOverrides - Apply partial JSON object onto an Options base

import Control.Exception (catch)
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Lazy as BL
import Data.Default
import MyPhoto.Model
import System.Directory (getHomeDirectory)

-- ############################################################################
-- ## JSON instances for strategy types
-- ############################################################################

instance A.ToJSON WorkdirStrategy where
  toJSON CreateNextToImgDir = A.String "CreateNextToImgDir"
  toJSON MoveExistingImgsToSubfolder = A.String "MoveExistingImgsToSubfolder"
  toJSON NextToImgFiles = A.String "NextToImgFiles"
  toJSON (ImportToWorkdir p) = A.object ["type" A..= ("ImportToWorkdir" :: String), "path" A..= p]
  toJSON (ImportToWorkdirWithSubdir p) = A.object ["type" A..= ("ImportToWorkdirWithSubdir" :: String), "path" A..= p]
  toJSON (WorkdirStrategyOverwrite p) = A.object ["type" A..= ("WorkdirStrategyOverwrite" :: String), "path" A..= p]

instance A.FromJSON WorkdirStrategy where
  parseJSON (A.String "CreateNextToImgDir") = pure CreateNextToImgDir
  parseJSON (A.String "MoveExistingImgsToSubfolder") = pure MoveExistingImgsToSubfolder
  parseJSON (A.String "NextToImgFiles") = pure NextToImgFiles
  parseJSON (A.Object v) = do
    t <- v A..: "type"
    case (t :: String) of
      "ImportToWorkdir" -> ImportToWorkdir <$> v A..: "path"
      "ImportToWorkdirWithSubdir" -> ImportToWorkdirWithSubdir <$> v A..: "path"
      "WorkdirStrategyOverwrite" -> WorkdirStrategyOverwrite <$> v A..: "path"
      _ -> fail ("Unknown WorkdirStrategy type: " ++ t)
  parseJSON _ = fail "Invalid WorkdirStrategy"

instance A.ToJSON ExportStrategy where
  toJSON NoExport = A.String "NoExport"
  toJSON Export = A.String "Export"
  toJSON ExportToParent = A.String "ExportToParent"

instance A.FromJSON ExportStrategy where
  parseJSON (A.String "NoExport") = pure NoExport
  parseJSON (A.String "Export") = pure Export
  parseJSON (A.String "ExportToParent") = pure ExportToParent
  parseJSON _ = fail "Invalid ExportStrategy: expected \"NoExport\", \"Export\", or \"ExportToParent\""

instance A.ToJSON CleanupStrategy where
  toJSON NoCleanup = A.String "NoCleanup"
  toJSON SomeCleanup = A.String "SomeCleanup"
  toJSON RemoveWorkdirRecursively = A.String "RemoveWorkdirRecursively"

instance A.FromJSON CleanupStrategy where
  parseJSON (A.String "NoCleanup") = pure NoCleanup
  parseJSON (A.String "SomeCleanup") = pure SomeCleanup
  parseJSON (A.String "RemoveWorkdirRecursively") = pure RemoveWorkdirRecursively
  parseJSON _ = fail "Invalid CleanupStrategy: expected \"NoCleanup\", \"SomeCleanup\", or \"RemoveWorkdirRecursively\""

instance A.ToJSON ChunkSettings where
  toJSON (ChunkSize n) = A.object ["type" A..= ("ChunkSize" :: String), "size" A..= n]
  toJSON (SparseChunksOfSize n) = A.object ["type" A..= ("SparseChunksOfSize" :: String), "size" A..= n]
  toJSON (ChunkTreeHeight n) = A.object ["type" A..= ("ChunkTreeHeight" :: String), "height" A..= n]
  toJSON NoChunks = A.String "NoChunks"

instance A.FromJSON ChunkSettings where
  parseJSON (A.String "NoChunks") = pure NoChunks
  parseJSON (A.Object v) = do
    t <- v A..: "type"
    case (t :: String) of
      "ChunkSize" -> ChunkSize <$> v A..: "size"
      "SparseChunksOfSize" -> SparseChunksOfSize <$> v A..: "size"
      "ChunkTreeHeight" -> ChunkTreeHeight <$> v A..: "height"
      _ -> fail ("Unknown ChunkSettings type: " ++ t)
  parseJSON _ = fail "Invalid ChunkSettings"

-- ############################################################################
-- ## JSON instances for Options
-- ############################################################################

instance A.ToJSON Options where
  toJSON Options {..} =
    A.object
      [ "verbose" A..= optVerbose,
        "redirectLog" A..= optRedirectLog,
        "workdirStrategy" A..= optWorkdirStrategy,
        "export" A..= optExport,
        "clean" A..= optClean,
        "everyNth" A..= optEveryNth,
        "sortOnCreateDate" A..= optSortOnCreateDate,
        "removeOutliers" A..= optRemoveOutliers,
        "breaking" A..= optBreaking,
        "untiff" A..= optUntiff,
        "unHeif" A..= optUnHeif,
        "downscalePct" A..= optDownscalePct,
        "align" A..= optAlign,
        "focusStack" A..= optFocusStack,
        "enfuse" A..= optEnfuse,
        "enfuseChunkSettings" A..= optEnfuseChunkSettings,
        "zereneStacker" A..= optZereneStacker,
        "zereneStackerHeadless" A..= optZereneStackerHeadless,
        "zereneStackerParallel" A..= optZereneStackerParallel,
        "zereneStackerChunkSettings" A..= optZereneStackerChunkSettings,
        "noGpu" A..= optNoGpu,
        "alignOutputs" A..= optAlignOutputs,
        "cropToCommonIntersectionFuzz" A..= optCropToCommonIntersectionFuzz,
        "parameters" A..= optParameters
      ]

instance A.FromJSON Options where
  parseJSON = A.withObject "Options" $ \v -> do
    let d = def :: Options
    Options
      <$> v A..:? "verbose" A..!= optVerbose d
      <*> v A..:? "redirectLog" A..!= optRedirectLog d
      <*> v A..:? "workdirStrategy" A..!= optWorkdirStrategy d
      <*> v A..:? "export" A..!= optExport d
      <*> v A..:? "clean" A..!= optClean d
      <*> v A..:? "everyNth" A..!= optEveryNth d
      <*> v A..:? "sortOnCreateDate" A..!= optSortOnCreateDate d
      <*> v A..:? "removeOutliers" A..!= optRemoveOutliers d
      <*> v A..:? "breaking" A..!= optBreaking d
      <*> v A..:? "untiff" A..!= optUntiff d
      <*> v A..:? "unHeif" A..!= optUnHeif d
      <*> v A..:? "downscalePct" A..!= optDownscalePct d
      <*> v A..:? "align" A..!= optAlign d
      <*> v A..:? "focusStack" A..!= optFocusStack d
      <*> v A..:? "enfuse" A..!= optEnfuse d
      <*> v A..:? "enfuseChunkSettings" A..!= optEnfuseChunkSettings d
      <*> v A..:? "zereneStacker" A..!= optZereneStacker d
      <*> v A..:? "zereneStackerHeadless" A..!= optZereneStackerHeadless d
      <*> v A..:? "zereneStackerParallel" A..!= optZereneStackerParallel d
      <*> v A..:? "zereneStackerChunkSettings" A..!= optZereneStackerChunkSettings d
      <*> v A..:? "alignOutputs" A..!= optAlignOutputs d
      <*> v A..:? "noGpu" A..!= optNoGpu d
      <*> v A..:? "cropToCommonIntersectionFuzz" A..!= optCropToCommonIntersectionFuzz d
      <*> v A..:? "parameters" A..!= optParameters d

-- ############################################################################
-- ## Config loading
-- ############################################################################

-- | Get the myphoto config directory (~/.myphoto)
configDir :: IO FilePath
configDir = do
  home <- getHomeDirectory
  return (home </> ".myphoto")

-- | Generic JSON config loader. Reads a JSON file and decodes it.
--   Returns the fallback value if the file does not exist or cannot be parsed.
loadJsonConfig :: (A.FromJSON a) => FilePath -> a -> IO a
loadJsonConfig path fallback = do
  exists <- doesFileExist path
  if not exists
    then return fallback
    else do
      logInfoIO ("Loading config from " ++ path)
      catch
        ( do
            bs <- BL.readFile path
            case A.eitherDecode bs of
              Left err -> do
                logWarnIO ("Failed to parse " ++ path ++ ": " ++ err)
                return fallback
              Right val -> return val
        )
        ( \e -> do
            logWarnIO ("Failed to read " ++ path ++ ": " ++ show (e :: IOError))
            return fallback
        )

-- | Load Options from ~/.myphoto/options.json.
--   Returns 'def' if the file does not exist or cannot be parsed.
loadOptionsConfig :: IO Options
loadOptionsConfig = do
  dir <- configDir
  loadJsonConfig (dir </> "options.json") def

-- | Apply partial Options overrides from a JSON object onto a base Options.
--   Only fields present in the object override the base; missing fields are
--   kept from the base.
applyOptionsOverrides :: A.Object -> Options -> Options
applyOptionsOverrides obj base =
  let applyField :: (A.FromJSON a) => A.Key -> (Options -> a -> Options) -> Options -> Options
      applyField key setter opts = case A.parseMaybe (A..: key) obj of
        Nothing -> opts
        Just val -> setter opts val
   in applyField "verbose" (\o v -> o {optVerbose = v})
        . applyField "redirectLog" (\o v -> o {optRedirectLog = v})
        . applyField "workdirStrategy" (\o v -> o {optWorkdirStrategy = v})
        . applyField "export" (\o v -> o {optExport = v})
        . applyField "clean" (\o v -> o {optClean = v})
        . applyField "everyNth" (\o v -> o {optEveryNth = v})
        . applyField "sortOnCreateDate" (\o v -> o {optSortOnCreateDate = v})
        . applyField "removeOutliers" (\o v -> o {optRemoveOutliers = v})
        . applyField "breaking" (\o v -> o {optBreaking = v})
        . applyField "untiff" (\o v -> o {optUntiff = v})
        . applyField "unHeif" (\o v -> o {optUnHeif = v})
        . applyField "downscalePct" (\o v -> o {optDownscalePct = v})
        . applyField "align" (\o v -> o {optAlign = v})
        . applyField "focusStack" (\o v -> o {optFocusStack = v})
        . applyField "enfuse" (\o v -> o {optEnfuse = v})
        . applyField "enfuseChunkSettings" (\o v -> o {optEnfuseChunkSettings = v})
        . applyField "zereneStacker" (\o v -> o {optZereneStacker = v})
        . applyField "zereneStackerHeadless" (\o v -> o {optZereneStackerHeadless = v})
        . applyField "zereneStackerParallel" (\o v -> o {optZereneStackerParallel = v})
        . applyField "zereneStackerChunkSettings" (\o v -> o {optZereneStackerChunkSettings = v})
        . applyField "noGpu" (\o v -> o {optNoGpu = v})
        . applyField "cropToCommonIntersectionFuzz" (\o v -> o {optCropToCommonIntersectionFuzz = v})
        . applyField "parameters" (\o v -> o {optParameters = v})
        $ base
