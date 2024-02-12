module MyPhoto.Model
  ( module FilePath,
    module Directory,
    module Exit,
    module Maybe,
    module Monad,
    module Map,
    module List,
    module Default,
    module Char,
    module X,
    Img,
    Imgs,
    WorkdirStrategy (..),
    ChunkSettings (..),
    Options (..),
    computeStackOutputBN,
    inWorkdir,
    findOutFile,
    findAltFileOfFile,
    logDebugIO,
    logInfoIO,
    logWarnIO,
    logErrorIO,
    ChunkSettings,
  )
where

import Control.Monad as Monad (unless, when)
import Data.Char as Char (toLower)
import Data.Default as Default
import Data.List as List (partition, sortOn)
import Data.Map as Map (Map (..))
import qualified Data.Maybe as Maybe (fromJust, isJust, mapMaybe, maybe)
import System.Directory as Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory, makeAbsolute, setCurrentDirectory)
import System.Exit as Exit (ExitCode (..), exitWith)
import System.FilePath as FilePath (makeRelative, splitExtensions, splitFileName, takeBaseName, takeDirectory, takeExtension, takeFileName, dropTrailingPathSeparator, (-<.>), (<.>), (</>))
import qualified System.IO as IO
import System.Posix.LoadAvg (LoadAvg (..), getLoadAvgSafe)
import MyPhoto.Utils.Samples as X

logDebugIO :: String -> IO ()
logDebugIO msg = IO.hPutStrLn IO.stderr ("DEBUG: " ++ msg)

logInfoIO :: String -> IO ()
logInfoIO msg = IO.hPutStrLn IO.stderr ("INFO: " ++ msg)

logWarnIO :: String -> IO ()
logWarnIO msg = IO.hPutStrLn IO.stderr ("WARN: " ++ msg)

logErrorIO :: String -> IO ()
logErrorIO msg = IO.hPutStrLn IO.stderr ("ERROR: " ++ msg)

type Img = FilePath

type Imgs = [FilePath]

data WorkdirStrategy
  = CreateNextToImgDir
  | MoveExistingImgsToSubfolder
  | NextToImgFiles
  | ImportToWorkdir FilePath
  | -- | ParentOfImgFiles
    WorkdirStrategyOverwrite FilePath
  deriving (Show, Eq)

data ChunkSettings
  = ChunkSize Int
  | NoChunks
  deriving (Show, Eq)

instance Default ChunkSettings where
  def = ChunkSize 8


data Options = Options
  { optVerbose :: Bool,
    optRedirectLog :: Bool,
    optWorkdirStrategy :: WorkdirStrategy,
    optEveryNth :: Maybe Int,
    optSortOnCreateDate :: Bool,
    optRemoveOutliers :: Bool,
    optBreaking :: Maybe Int,
    optUntiff :: Bool,
    optUnHeif :: Bool,
    optFocusStack :: Bool,
    optFocusStackBatchSize :: ChunkSettings,
    optEnfuse :: Bool,
    optEnfuseChunkSettings :: ChunkSettings,
    optParameters :: Map String [String]
  }
  deriving (Show)

computeStackOutputBN :: [FilePath] -> FilePath
computeStackOutputBN [] = undefined -- should not happen
computeStackOutputBN (img0 : oimgs) =
  let lastImg = case oimgs of
        [] -> ""
        _ -> "_to_" ++ takeBaseName (last oimgs)
   in takeBaseName img0 ++ lastImg ++ "_stack_of_" ++ show (length oimgs + 1)

inWorkdir :: FilePath -> FilePath -> FilePath
inWorkdir workdir img = workdir </> takeFileName img

findOutFile :: String -> String -> IO FilePath
findOutFile bn ext =
  let findOutFile' :: Int -> IO FilePath
      findOutFile' i = do
        let fn = bn ++ "_" ++ show i ++ ext
        exists <- doesFileExist fn
        if exists
          then findOutFile' (i + 1)
          else return fn
      touchFile :: FilePath -> IO FilePath
      touchFile fn = do
        writeFile fn ""
        return fn
   in do
        let fn = bn ++ ext
        exists <- doesFileExist fn
        ( if exists
            then findOutFile' 1
            else return fn
          )
          >>= touchFile

findAltFileOfFile :: FilePath -> IO FilePath
findAltFileOfFile file =
  let (bn, ext) = splitExtensions file
   in findOutFile bn ext
