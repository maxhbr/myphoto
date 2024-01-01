module MyPhoto.Model 
    ( module FilePath
    , module Directory
    , Options(..)
    , getOptions
    , setCurrentWD
    , computeStackOutputBN
    , findOutFile
    )where

import           System.FilePath            as FilePath (takeBaseName, takeFileName, takeDirectory, (<.>), (-<.>), (</>))
import           Control.Monad              (unless, when)
import           System.Console.GetOpt
import           System.Directory           as Directory (createDirectoryIfMissing, makeAbsolute, doesFileExist, setCurrentDirectory)
import           System.Environment         (getArgs, withArgs, getProgName)
import           System.Exit                    ( ExitCode(..), exitWith )
import qualified System.IO                  as IO
import           System.Posix.LoadAvg       (LoadAvg (..), getLoadAvgSafe)

data Options = Options  { optVerbose   :: Bool
                        , optWorkdir   :: Maybe FilePath 
                        }
                        deriving (Show)
startOptions :: Options
startOptions = Options  { optVerbose    = False
                             , optWorkdir    = Nothing
                             }
options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option "w" ["workdir"]
        (ReqArg
            (\arg opt -> return opt { optWorkdir = Just arg})
            "DIR")
        "work directory"

    , Option "v" ["verbose"]
        (NoArg
            (\opt -> return opt { optVerbose = True }))
        "Enable verbose messages"

    , Option "h" ["help"]
        (NoArg
            (\_ -> do
             prg <- getProgName
             IO.hPutStr IO.stderr (usageInfo prg options)
             IO.hPutStrLn IO.stderr "  IMG0 [IMG1]..."
             exitWith ExitSuccess))
        "Show help"
    ]


computeStackOutputBN :: [FilePath] -> FilePath
computeStackOutputBN [] = undefined -- should not happen
computeStackOutputBN (img0:oimgs) = let
    lastImg = case oimgs of
        [] -> ""
        _  -> "_to_" ++ takeBaseName (last oimgs)
  in takeBaseName img0 ++ lastImg ++ "_stack_of_" ++ show (length oimgs + 1)


getOptions :: IO (Options, [FilePath])
getOptions = let
        makeImgPathsAbsolute :: [FilePath] -> IO [FilePath]
        makeImgPathsAbsolute imgs = mapM makeAbsolute imgs

        fillWorkDir :: [FilePath] -> Options -> IO Options
        fillWorkDir imgs opts@Options{optWorkdir = Just wd} = do
            absWd <- makeAbsolute wd
            return opts{optWorkdir = Just absWd}
        fillWorkDir imgs@(img:_) opts = return opts{optWorkdir = let
                imgBN = computeStackOutputBN imgs
            in Just $ (takeDirectory img) <.> imgBN <.> "myphoto"}
        fillWorkDir _ _ = undefined -- should not happen
    in do
        args <- getArgs
        let (actions, imgs', errors) = getOpt RequireOrder options args
        mapM_ (IO.hPutStrLn IO.stderr) errors

        imgs <- makeImgPathsAbsolute imgs'

        opts' <- foldl (>>=) (return startOptions) actions

        when (null imgs)$ do
            IO.hPutStrLn IO.stderr ("no image specified: " ++ show imgs)
            exitWith (ExitFailure 1)

        opts <- fillWorkDir imgs opts'

        mapM_ (\img -> do
            exists <- doesFileExist img
            unless exists $ do
                IO.hPutStrLn IO.stderr $ "image not found: " ++ img
                exitWith (ExitFailure 1)
            ) imgs

        return (opts, imgs)

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
