module MyPhoto.Actions.Outliers
    ( rmOutliers
    ) where

import           Control.Concurrent.Async ( mapConcurrently )
import           Control.Concurrent.MSem as MS
import           Control.Monad
import           GHC.Conc ( numCapabilities )
import           System.Console.GetOpt
import           System.IO.Temp
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.Process
import           Graphics.Netpbm
import qualified Data.ByteString as B
import           Data.Maybe (maybe)
import           Statistics.Sample (variance, stdDev)
import qualified Data.Vector as V (fromList)

import MyPhoto.Model
import MyPhoto.Utils


help :: String
help = let
    header = "Usage: rmoutliers [OPTION...] files..."
  in usageInfo header options

data Options
  = Options
  { optMaxDistance :: Double
  , optSize        :: Int
  , optHelp        :: Bool
  } deriving Show

defaultOptions :: Options
defaultOptions
  = Options
  { optMaxDistance = 200
  , optSize        = 6
  , optHelp        = False
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['d'] ["maxDistance"]
      (OptArg ((\ d opts -> opts { optMaxDistance = d }) . maybe (optMaxDistance defaultOptions) read)
        "DISTANCE")
      "maximum distance to allow"
  , Option ['s'] ["size"]
      (OptArg ((\ s opts -> opts { optSize = s }) . maybe (optSize defaultOptions) read)
        "SIZE")
      "size of reduced image"
  , Option ['h'] ["help"]
      (NoArg (\ opts -> opts { optHelp = True }))
      "print help"
  ]

getMyOpts :: [String] -> IO (Options, [String])
getMyOpts argv = case getOpt Permute options argv of
                   (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
                   (_,_,errs) -> ioError (userError (concat errs ++ help))

computImgVecs :: Int -> FilePath -> Img -> IO (Img, [Int])
computImgVecs size tmpdir img = do
  let ppmFile = tmpdir </> (takeFileName img ++ ".ppm")

  (_, _, _, pHandle) <- createProcess (proc "convert" [img, "-resize", show size ++ "x" ++ show size ++ "!", ppmFile])
  exitCode <- waitForProcess pHandle
  unless (exitCode == ExitSuccess) $
    fail ("Resize failed with " ++ show exitCode)


  ppmResult <- parsePPM <$> B.readFile ppmFile
  case ppmResult of
    Right ([ppmImg],_) -> let
        imgVec = (pixelDataToIntList . ppmData) ppmImg
      in return (img, imgVec)
    Left err           -> fail ("PPM parsing failed with " ++ err)
    _                  -> fail "PPM parsing failed"

calculateDistances :: [(Img, [Int])] -> IO [(Img, [Int], Double)]
calculateDistances imgsWithVecs = let
    calculateDistance :: [Int] -> [Int] -> Double
    calculateDistance vec1 vec2 =
      sqrt .
      fromIntegral .
      sum .
      map ((\v -> v*v) . uncurry (-)) $ zip vec1 vec2
    foldFun :: [(Img, [Int], Double)] -> (Img, [Int]) -> [(Img, [Int], Double)]
    foldFun [] (img, vec)   = [(img, vec, 0)]
    foldFun prev (img, vec) = let
        (_, prevVec, _) = last prev
        dist = calculateDistance prevVec vec
      in prev ++ [(img, vec, dist)]
    imgsWithDists = foldl foldFun [] imgsWithVecs
    dists = map (\(_,_,d) -> d) imgsWithDists
  in do
  putStrLn $ "Variance = " ++ show (variance (V.fromList dists))
  putStrLn $ "Standard deviation =" ++ show (stdDev (V.fromList dists))
  return imgsWithDists


rmOutliersImpl :: [String] -> [Img] -> PActionBody
rmOutliersImpl _    []            = return (Right [])
rmOutliersImpl args imgs@(img1:_) = let
    dropByDistances :: Double -> [(Img, [Int], Double)] -> [Int] -> IO [Img]
    dropByDistances _ []                                 _       = return []
    dropByDistances maxDist ((img, curVec, dist):imgsWithVecs) lastVec =
      if dist < maxDist
      then do
        fmap (img :) (dropByDistances maxDist imgsWithVecs curVec)
      else do
        putStrLn ("drop " ++ img ++ " with dist " ++ show dist)
        dropByDistances maxDist imgsWithVecs lastVec
  in do
    (opts, _) <- getMyOpts args
    print opts
    if optHelp opts
    then return (Left help)
    else withTempDirectory (takeDirectory img1) "_outliers.tmp"
           (\tmpdir -> do
               imgsWithVecs <- mapConcurrently (computImgVecs (optSize opts) tmpdir) imgs
               imgsWithDists <- calculateDistances imgsWithVecs
               imgsWithoutOutliers <- dropByDistances (optMaxDistance opts) imgsWithDists []
               return (Right imgsWithoutOutliers)
             )
rmOutliers :: PrePAction
rmOutliers args     = logSeparator "rmoutliers" <> PAction (rmOutliersImpl args)
