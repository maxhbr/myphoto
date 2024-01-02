module MyPhoto.Model 
    ( module FilePath
    , module Directory
    , module Exit
    , Img
    , Imgs
    , Options(..)
    , setCurrentWD
    , computeStackOutputBN
    , findOutFile
    )where

import           System.FilePath            as FilePath (takeBaseName, takeFileName, takeDirectory, (<.>), (-<.>), (</>))
import           Control.Monad              (unless, when)
import           System.Directory           as Directory (createDirectoryIfMissing, makeAbsolute, doesFileExist, doesDirectoryExist, setCurrentDirectory, listDirectory)
import           System.Exit                as Exit ( ExitCode(..), exitWith )
import qualified System.IO                  as IO
import           System.Posix.LoadAvg       (LoadAvg (..), getLoadAvgSafe)

type Img = FilePath
type Imgs = [FilePath]

data Options = Options  { optVerbose   :: Bool
                        , optWorkdir   :: Maybe FilePath 
                        , optEveryNth  :: Maybe Int
                        , optBreaking  :: Maybe Int
                        , optEnfuse    :: Bool
                        }
                        deriving (Show)

computeStackOutputBN :: [FilePath] -> FilePath
computeStackOutputBN [] = undefined -- should not happen
computeStackOutputBN (img0:oimgs) = let
    lastImg = case oimgs of
        [] -> ""
        _  -> "_to_" ++ takeBaseName (last oimgs)
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


findOutFile :: String -> String -> IO FilePath
findOutFile bn ext = let
  findOutFile' :: Int -> IO FilePath
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
    (if exists
      then findOutFile' 1
      else return fn) >>= touchFile
