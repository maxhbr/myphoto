module MyPhoto.Model
  ( module FilePath,
    module Directory,
    module Exit,
    module Maybe,
    module Monad,
    module Map,
    module List,
    Img,
    Imgs,
    WorkdirStrategy (..),
    Options (..),
    computeStackOutputBN,
    inWorkdir,
    findOutFile,
    findAltFileOfFile,
  )
where

import Control.Monad as Monad (unless, when)
import Data.Map as Map (Map (..))
import Data.List as List (sortOn)
import qualified Data.Maybe as Maybe (fromJust, isJust, mapMaybe, maybe)
import System.Directory as Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory, makeAbsolute, setCurrentDirectory)
import System.Exit as Exit (ExitCode (..), exitWith)
import System.FilePath as FilePath (makeRelative, splitExtensions, splitFileName, takeBaseName, takeDirectory, takeFileName, (-<.>), (<.>), (</>))
import qualified System.IO as IO
import System.Posix.LoadAvg (LoadAvg (..), getLoadAvgSafe)

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

data Options = Options
  { optVerbose :: Bool,
    optRedirectLog :: Bool, 
    optWorkdirStrategy :: WorkdirStrategy,
    optEveryNth :: Maybe Int,
    optSortOnCreateDate :: Bool,
    optRemoveOutliers :: Bool,
    optBreaking :: Maybe Int,
    optFocusStack :: Bool,
    optEnfuse :: Bool,
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
