module MyPhoto.MyPhoto
  ( runMyPhoto
  ) where

import           Control.Monad              (unless, when)
import qualified System.IO                  as IO
import           System.Console.GetOpt
import           System.Environment         (getArgs, withArgs, getProgName)
import qualified Control.Monad.State.Lazy as MTL

import MyPhoto.Model

import MyPhoto.Actions.FocusStack 
import MyPhoto.Actions.Enfuse
import MyPhoto.Actions.Exiftool
import MyPhoto.Actions.Montage
import MyPhoto.Actions.Outliers
import MyPhoto.Actions.Align

startOptions :: Options
startOptions = Options  { optVerbose    = False
                        , optWorkdir    = Nothing
                        , optEveryNth   = Nothing
                        , optRemoveOutliers = True
                        , optBreaking   = Just 300
                        , optEnfuse     = True
                        , optFocusStack = True
                        }

options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option "w" ["workdir"]
        (ReqArg
            (\arg opt -> return opt { optWorkdir = Just arg})
            "DIR")
        "work directory"

    , Option "" ["no-enfuse"]
        (NoArg
            (\opt -> return opt { optEnfuse = False }))
        "Do not run enfuse"

    , Option "" ["enfuse"]
        (NoArg
            (\opt -> return opt { optEnfuse = True }))
        "Run enfuse (default)"

    , Option "" ["no-focus-stack"]
        (NoArg
            (\opt -> return opt { optFocusStack = False }))
        "Do not run focus stacking by PetteriAimonen/focus-stack"

    , Option "" ["every-nth"]
        (ReqArg
            (\arg opt -> return opt { optEveryNth = Just (read arg :: Int)})
            "N")
        "just take every n-th image (at least first and last)"
    
    , Option "" ["no-remove-outliers"]
        (NoArg
            (\opt -> return opt { optRemoveOutliers = False }))
        "Do not remove outliers"

    , Option "" ["remove-outliers"]
        (NoArg
            (\opt -> return opt { optRemoveOutliers = True }))
        "Remove outliers (default)"

    , Option "" ["breaking"]
        (ReqArg
            (\arg opt -> return opt { optBreaking = Just (read arg :: Int)})
            "SECONDS")
        "break on time gap"

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

getOptionsAndInitialize :: IO (Options, [FilePath])
getOptionsAndInitialize = let
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

        everyNth :: Int -> [a] -> [a]
        everyNth _ [] = []
        everyNth n (x:xs) = let
            everyNth' :: Int -> [a] -> [a]
            everyNth' n xs = case drop (n-1) xs of
              (y:ys) -> y : everyNth n ys
              [] -> [last xs | not (null xs)]
          in x : everyNth' n xs

        applySparse :: Options -> [FilePath] -> [FilePath]
        applySparse Options{optEveryNth = Just n} imgs = everyNth n imgs
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

        imgs <- makeImgPathsAbsolute (applySparse opts' imgs')
        print imgs
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

myPhotoStateAction :: Options -> MTL.StateT (Imgs,Imgs) IO ()
myPhotoStateAction opts@Options{optVerbose = verbose} = do
    case optBreaking opts of
      Nothing -> return ()
      Just gapInSeconds -> do
        (imgs, outs) <- MTL.get
        broken <- MTL.liftIO $ breaking gapInSeconds imgs
        MTL.put (broken, outs)

    when (optRemoveOutliers opts) $ do
      (imgs, outs) <- MTL.get
      withoutOutliers <- MTL.liftIO $ rmOutliers (optWorkdir opts) imgs
      MTL.put (withoutOutliers, outs)

    outputBN <- do
      imgs <- MTL.gets fst
      return (computeStackOutputBN imgs)

    if optFocusStack opts
    then do
        (imgs, outs) <- MTL.get
        (focusStacked,aligned) <- MTL.liftIO $ focusStackImgs opts imgs
        MTL.put (aligned, outs ++ [focusStacked])
        return aligned
    else do
        (imgs, outs) <- MTL.get
        aligned <- MTL.liftIO $ align opts imgs
        MTL.put (aligned, outs)
        return aligned

      
    when (optEnfuse opts) $ do
      (imgs, outs) <- MTL.get
      enfuseResult <- MTL.liftIO $ enfuseStackImgs (enfuseDefaultOptions
                        { optOutputBN = Just outputBN
                        , optEnfuseVerbose = verbose
                      }) imgs
      case enfuseResult of
        Left err -> MTL.liftIO $ IO.hPutStrLn IO.stderr err
        Right enfuseOuts -> MTL.put (imgs, outs ++ enfuseOuts)

    do 
      (_, outs) <- MTL.get
      montageOut <- MTL.liftIO $ montage (inWorkdir opts outputBN) outs
      MTL.liftIO $ IO.hPutStrLn IO.stderr ("INFO: wrote " ++ montageOut)

    return ()

runMyPhoto :: IO ()
runMyPhoto = do
    (opts@Options{optVerbose = verbose}, imgs) <- getOptionsAndInitialize
    setCurrentWD opts
    when verbose $ print opts
    
    (ret,(imgs', outs)) <- MTL.runStateT (myPhotoStateAction opts) (imgs, []) 

    IO.hPutStrLn IO.stderr ("Done")
