module MyPhoto.Actions.Align
  ( align
  ) where

import           System.FilePath
import           System.Console.GetOpt
import           Control.Monad
import           Data.Maybe (fromMaybe)
import           System.Process
import           System.Exit
import           System.Directory
import           System.IO.Temp
import           Text.Printf

import MyPhoto.Model
import MyPhoto.Utils

data Options
  = Options
  { optVerbose      :: Bool
  , optHelp         :: Bool
  } deriving Show

defaultOptions :: Options
defaultOptions
  = Options
  { optVerbose      = False
  , optHelp         = False
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['v'] ["verbose"]
      (NoArg (\ opts -> opts { optVerbose = True }))
      "chatty output on stderr"
  , Option ['h'] ["help"]
      (NoArg (\ opts -> opts { optHelp = True }))
      "print help"
  ]


help :: String
help = let
    header = "Usage: align [OPTION...] files..."
  in usageInfo header options

getMyOpts :: [String] -> IO (Options, [String])
getMyOpts argv = case getOpt Permute options argv of
                   (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
                   (_,_,errs) -> ioError (userError (concat errs ++ help))

callAlignImageStack :: [String] -> String -> [Img] -> IO [Img]
callAlignImageStack alignArgs prefix imgs = let
    args = alignArgs ++ ["-a", prefix]
  in do
    (_, _, _, pHandle) <- createProcess (proc "align_image_stack" (args ++ imgs))
    exitCode <- waitForProcess pHandle
    when (exitCode /= ExitSuccess) $
      fail ("callAlignImageStack failed with" ++ show exitCode)

    return (map (printf (prefix ++ "%04d.tif")) [0..(length imgs - 1)])

callAlignImageStackByHalves :: [String] -> FilePath -> [Img] -> IO [Img]
callAlignImageStackByHalves args tmpdir imgs = let
    imgsLen   = length imgs
    firstImgs = take (imgsLen `div` 2 + 1) imgs
    lastImgs  = drop (imgsLen `div` 2) imgs
  in if imgsLen < 4
     then callAlignImageStack args (tmpdir </> "fwd_") imgs
     else do
       putStrLn ("align images from " ++ head firstImgs ++ " to " ++ last firstImgs ++ " (#=" ++ show (length firstImgs) ++ ")")
       bwd <- callAlignImageStack args (tmpdir </> "bwd_") (reverse firstImgs)
       putStrLn ("align images from " ++ head lastImgs ++ " to " ++ last lastImgs ++ " (#=" ++ show (length lastImgs) ++ ")")
       fwd <- callAlignImageStack args  (tmpdir </> "fwd_") lastImgs
       return (reverse (tail bwd) ++ fwd)

copyAndRenameImages :: (Int -> String) -> [Img] -> IO [Img]
copyAndRenameImages renamer imgs = mapM (\(img, i) -> do
                                            let out = renamer i
                                            altOut <- findAltFileOfFile out
                                            copyFile img altOut
                                            return altOut) (zip imgs [1..])

alignImpl :: [String] -> [Img] -> PActionBody
alignImpl args imgs = do
  (opts, _) <- getMyOpts args
  -- TODO: look at: https://photo.stackexchange.com/a/83179
  let alignArgs = [ "-v" | optVerbose opts ]
               ++ [ "--use-given-order"
                 , "-l" -- Assume linear input files
                 , "-c", "20" -- number of control points (per grid) to create between adjacent images
                 , "-s", "2"  -- Scale down image by 2^scale (default: 1 [2x downsampling])
                 -- , "-i" -- Optimize image center shift for all images, except for first.
                 -- , "-m" -- Optimize field of view for all images, except for first. Useful for aligning focus stacks with slightly different magnification.
                 , "--gpu" -- Use GPU for remapping
                 ]

  let prefix = dropExtension (head imgs)
      mkOutImgName :: Int -> String
      mkOutImgName i = printf (prefix ++ "_ALIGN-%04d-%04d.tif") i (length imgs)

  withSystemTempDirectory "myphoto.tmp"
    (\tmpdir -> do
        imgsInTmp <- callAlignImageStackByHalves alignArgs tmpdir imgs
        mapM_ (\fn -> do
                  fnExists <- doesFileExist fn
                  unless fnExists $
                    fail ("the file " ++ fn ++ " should exist after align")
              ) imgsInTmp
        outs <- copyAndRenameImages mkOutImgName imgsInTmp
        return (Right outs)
      )

align :: PrePAction
align ["-h"] = PAction (\_ -> pure (Left help))
align args = logSeparator "Run align" <> PAction (alignImpl args)
