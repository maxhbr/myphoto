module MyPhoto.Actions.Outliers
  ( rmOutliers,
  )
where

import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.MSem as MS
import Control.Monad
import qualified Data.ByteString as B
import Data.Maybe (maybe)
import qualified Data.Vector as V (fromList)
import GHC.Conc (numCapabilities)
import Graphics.Netpbm
import MyPhoto.Model
import MyPhoto.Utils
import qualified Statistics.Sample as S
import System.Console.GetOpt
import System.Directory
import System.Exit
import System.FilePath
import System.IO.Temp
import System.Process

help :: String
help =
  let header = "Usage: rmoutliers [OPTION...] files..."
   in usageInfo header options

data Options = Options
  { optMaxDistance :: Double,
    optSize :: Int,
    optHelp :: Bool
  }
  deriving (Show)

defaultOptions :: Options
defaultOptions =
  Options
    { optMaxDistance = 200,
      optSize = 6,
      optHelp = False
    }

options :: [OptDescr (Options -> Options)]
options =
  [ Option
      ['d']
      ["maxDistance"]
      ( OptArg
          ((\d opts -> opts {optMaxDistance = d}) . maybe (optMaxDistance defaultOptions) read)
          "DISTANCE"
      )
      "maximum distance to allow",
    Option
      ['s']
      ["size"]
      ( OptArg
          ((\s opts -> opts {optSize = s}) . maybe (optSize defaultOptions) read)
          "SIZE"
      )
      "size of reduced image",
    Option
      ['h']
      ["help"]
      (NoArg (\opts -> opts {optHelp = True}))
      "print help"
  ]

getMyOpts :: [String] -> IO (Options, [String])
getMyOpts argv = case getOpt Permute options argv of
  (o, n, []) -> return (foldl (flip id) defaultOptions o, n)
  (_, _, errs) -> ioError (userError (concat errs ++ help))

computImgVec :: Int -> FilePath -> Img -> IO (Img, [Int])
computImgVec size tmpdir img = do
  let ppmFile = tmpdir </> (takeFileName img ++ ".ppm")

  (_, _, _, pHandle) <- createProcess (proc "convert" [img, "-resize", show size ++ "x" ++ show size ++ "!", ppmFile])
  exitCode <- waitForProcess pHandle
  unless (exitCode == ExitSuccess) $
    fail ("Resize failed with " ++ show exitCode)

  ppmResult <- parsePPM <$> B.readFile ppmFile
  case ppmResult of
    Right ([ppmImg], _) ->
      let imgVec = (pixelDataToIntList . ppmData) ppmImg
       in return (img, imgVec)
    Left err -> fail ("PPM parsing failed with " ++ err)
    _ -> fail "PPM parsing failed"

computImgsVecs :: Int -> FilePath -> [Img] -> IO [(Img, [Int])]
computImgsVecs size tmpdir imgs = do
  capabilities <- getNumCapabilities
  sem <- MS.new (capabilities - 2)
  mapConcurrently (MS.with sem . computImgVec size tmpdir) imgs

rmOutliersImpl :: [String] -> [Img] -> PActionBody
rmOutliersImpl _ [] = return (Right [])
rmOutliersImpl args imgs@(img1 : _) =
  let dropByDistances :: Double -> [(Img, [Int])] -> [Int] -> IO [(Img, Double)]
      dropByDistances _ [] _ = return []
      dropByDistances maxDist ((img, curVec) : imgsWithVecs) lastVec =
        let calculateDistance :: [Int] -> [Int] -> Double
            calculateDistance vec1 vec2 =
              sqrt
                . fromIntegral
                . sum
                . map ((\v -> v * v) . uncurry (-))
                $ zip vec1 vec2
            dist = calculateDistance lastVec curVec
         in if dist < maxDist
              then do
                fmap ((img, dist) :) (dropByDistances maxDist imgsWithVecs curVec)
              else do
                putStrLn ("drop " ++ img ++ " with dist " ++ show dist)
                dropByDistances maxDist imgsWithVecs lastVec
      printStatsOnDists :: [(Img, Double)] -> IO ()
      printStatsOnDists imgsWithoutOutliers =
        let dists = V.fromList (map snd imgsWithoutOutliers)
         in do
              putStrLn $ "Variance = " ++ show (S.variance dists)
              putStrLn $ "Standard deviation = " ++ show (S.stdDev dists)
              putStrLn $ "Mean = " ++ show (S.mean dists)
   in do
        (opts, _) <- getMyOpts args
        print opts
        if optHelp opts
          then return (Left help)
          else
            withTempDirectory
              (takeDirectory img1)
              "_outliers.tmp"
              ( \tmpdir -> do
                  imgsWithVecs <- computImgsVecs (optSize opts) tmpdir imgs
                  imgsWithoutOutliers <- dropByDistances (optMaxDistance opts) imgsWithVecs []
                  printStatsOnDists imgsWithoutOutliers
                  return (Right (map fst imgsWithoutOutliers))
              )

rmOutliers :: PrePAction
rmOutliers args = logSeparator "rmoutliers" <> PAction (rmOutliersImpl args)
