module MyPhoto.Wrapper.EnblendEnfuseWrapper
  ( runEnblendEnfuse,
    runEnblendEnfuseWithRetries,
    Projection (..),
    EnfuseOpts (..),
    EnblendEnfuseOptions (..),
  )
where

import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.MSem as MS
import Control.Monad
import Data.Char (toLower)
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe)
import MyPhoto.Model hiding (Options (..))
import System.Console.GetOpt
import System.Directory
import System.Exit
import System.FilePath
import System.Process

data Projection
  = Proj1
  | Proj2
  | Proj3
  deriving (Show)

projectionToArgs :: Projection -> [String]
projectionToArgs Proj1 = ["--gray-projector=l-star"]
projectionToArgs Proj2 = []
projectionToArgs Proj3 = ["--gray-projector=luminance"]

data EnfuseOpts
  = Opts1
  | Opts2
  | Opts3
  deriving (Show)

optsToArgs :: EnfuseOpts -> [String]
optsToArgs Opts1 = ["--contrast-window-size=5"]
optsToArgs Opts2 = ["--contrast-edge-scale=0.3"]
optsToArgs Opts3 = ["--contrast-edge-scale=31", "--contrast-min-curvature=11"]

data EnblendEnfuseOptions = EnblendEnfuseOptions
  { eeVerbose :: Bool,
    eeProjection :: Projection,
    eeOpts :: EnfuseOpts,
    eeSaveMasks :: Bool,
    eeAdditionalArgs :: [String]
  }
  deriving (Show)

instance Default EnblendEnfuseOptions where
  def =
    EnblendEnfuseOptions
      { eeVerbose = False,
        eeProjection = Proj1,
        eeOpts = Opts1,
        eeSaveMasks = False,
        eeAdditionalArgs = []
      }

runEnblendEnfuse :: EnblendEnfuseOptions -> FilePath -> FilePath -> [FilePath] -> IO (Either ExitCode FilePath)
runEnblendEnfuse _ _ _ [img] = return (Right img)
runEnblendEnfuse opts@(EnblendEnfuseOptions {eeSaveMasks = saveMasks}) outFile workdir imgs = do
  logDebugIO (">>>>>>>>>>>>>>>>>>>>>>>> start >> " ++ (takeFileName outFile))
  let outMasksFolder = inWorkdir workdir (outFile ++ "-masks")
  when saveMasks $
    createDirectoryIfMissing True outMasksFolder
  let maskArgs = ["--save-masks=\"" ++ outMasksFolder ++ "/softmask-%04n.tif:" ++ outMasksFolder ++ "/hardmask-%04n.tif\"" | saveMasks]
      outputArgs = ["--output=" ++ outFile]
      focusStackArgs = ["--exposure-weight=0", "--saturation-weight=0", "--contrast-weight=1"]
      hardMaskArgs = ["--hard-mask"]
      verbosityArgs = ["-v" | eeVerbose opts]
      additionalArgs = eeAdditionalArgs opts
      args = concat [verbosityArgs, focusStackArgs, hardMaskArgs, projectionToArgs (eeProjection opts), optsToArgs (eeOpts opts), maskArgs, outputArgs, additionalArgs]
  logDebugIO (unwords ["$ enfuse", unwords args, "[img [img [...]]]"])
  (_, _, _, pHandle) <-
    createProcess
      (proc "enfuse" (args ++ imgs))
  exitCode <- waitForProcess pHandle
  case exitCode of
    ExitSuccess -> do
      logDebugIO ("<<<<<<<<<<<<<<<<<<<<<<<<< done << " ++ (takeFileName outFile))
      return (Right outFile)
    _ -> return (Left exitCode)

runEnblendEnfuseWithRetries :: Int -> EnblendEnfuseOptions -> FilePath -> FilePath -> [FilePath] -> IO (Either String FilePath)
runEnblendEnfuseWithRetries _ _ _ _ [img] = return (Right img)
runEnblendEnfuseWithRetries retriesLeft opts outFile workdir imgs = do
  result <- runEnblendEnfuse opts outFile workdir imgs
  case result of
    Right outs -> do
      return (Right outs)
    Left exitCode -> do
      let msg = "Stack of " ++ (takeFileName outFile) ++ " failed with " ++ show exitCode
      if retriesLeft > 0
        then do
          logWarnIO ("### " ++ msg ++ " (retrying)")
          runEnblendEnfuseWithRetries (retriesLeft - 1) opts outFile workdir imgs
        else do
          logErrorIO ("### " ++ msg ++ " (giving up)")
          return (Left msg)
