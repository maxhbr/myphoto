module MyPhoto.Model
  ( module FilePath,
    module Directory,
    module Exit,
    module Maybe,
    Img,
    Imgs,
    FSAction (..),
    Options (..),
    setCurrentWD,
    computeStackOutputBN,
    inWorkdir',
    inWorkdir,
    findOutFile,
    findAltFileOfFile,
  )
where

import Control.Monad (unless, when)
import qualified Data.Maybe as Maybe (isJust, fromJust, mapMaybe, maybe)
import System.Directory as Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory, makeAbsolute, setCurrentDirectory)
import System.Exit as Exit (ExitCode (..), exitWith)
import System.FilePath as FilePath (splitExtensions, takeBaseName, takeDirectory, takeFileName, (-<.>), (<.>), (</>))
import qualified System.IO as IO
import System.Posix.LoadAvg (LoadAvg (..), getLoadAvgSafe)

type Img = FilePath

type Imgs = [FilePath]

data FSAction
  = NoFSAction
  | FSActionCopy
  | FSActionMove
  | FSActionLink
  | FSActionReverseLink
  deriving (Show, Eq)

data Options = Options
  { optVerbose :: Bool,
    optFSAction :: FSAction,
    optWorkdir :: Maybe FilePath,
    optEveryNth :: Maybe Int,
    optRemoveOutliers :: Bool,
    optBreaking :: Maybe Int,
    optFocusStack :: Bool,
    optEnfuse :: Bool
  }
  deriving (Show)

computeStackOutputBN :: [FilePath] -> FilePath
computeStackOutputBN [] = undefined -- should not happen
computeStackOutputBN (img0 : oimgs) =
  let lastImg = case oimgs of
        [] -> ""
        _ -> "_to_" ++ takeBaseName (last oimgs)
   in takeBaseName img0 ++ lastImg ++ "_stack_of_" ++ show (length oimgs + 1)

setCurrentWD :: Options -> IO ()
setCurrentWD opts = do
  case optWorkdir opts of
    Just wd -> do
      IO.hPutStrLn IO.stderr $ "INFO: work directory: " ++ wd
      createDirectoryIfMissing True wd
      setCurrentDirectory wd
    Nothing -> do
      IO.hPutStrLn IO.stderr "no work directory specified"
      exitWith (ExitFailure 1)

inWorkdir' :: FilePath -> FilePath -> FilePath
inWorkdir' workdir img = workdir </> takeFileName img

inWorkdir :: Options -> FilePath -> FilePath
inWorkdir opts img = case optWorkdir opts of
  Just workdir -> inWorkdir' workdir img
  Nothing -> img

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
