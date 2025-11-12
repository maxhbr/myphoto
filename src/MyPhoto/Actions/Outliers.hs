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
import qualified Statistics.Sample as S
import System.Console.GetOpt
import System.Directory
import System.Exit
import System.FilePath
import System.IO.Temp
import System.Process
import System.ProgressBar (ProgressBar, defStyle, newProgressBar, incProgress, Progress (..))

computImgVec :: Int -> FilePath -> Img -> IO (Img, [Int])
computImgVec size tmpdir img = do
  let ppmFile = tmpdir </> (takeFileName img ++ ".ppm")

  (_, _, _, pHandle) <- createProcess (proc "magick" [img, "-resize", show size ++ "x" ++ show size ++ "!", ppmFile])
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
  let actualCapabilities = if capabilities > 4 then capabilities - 4 else capabilities
  putStrLn $ "Using " ++ show actualCapabilities ++ " (of " ++ show capabilities ++ ") concurrent threads for outlier detection"
  sem <- MS.new actualCapabilities
  pb <- newProgressBar defStyle 10 (Progress 0 (length imgs) ())
  mapConcurrently (\img -> MS.with sem $ do
    vec <- computImgVec size tmpdir img
    incProgress pb 1
    return vec
    ) imgs

rmOutliers :: FilePath -> Imgs -> IO Imgs
rmOutliers _ [] = return []
rmOutliers workdir imgs =
  let (maxDistance, size) = (200, 6)
      dropByDistances :: Double -> [(Img, [Int])] -> [Int] -> IO [(Img, Double)]
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
        withTempDirectory
          workdir
          "_outliers.tmp"
          ( \tmpdir -> do
              imgsWithVecs <- computImgsVecs size tmpdir imgs
              imgsWithoutOutliers <- dropByDistances maxDistance imgsWithVecs []
              printStatsOnDists imgsWithoutOutliers
              return (map fst imgsWithoutOutliers)
          )
