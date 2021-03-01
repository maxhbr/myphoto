module MyPhoto.Actions.Outliers
    ( rmOutliers
    ) where

import           Control.Concurrent.Async ( mapConcurrently )
import           Control.Concurrent.MSem as MS
import           Control.Monad
import           GHC.Conc ( numCapabilities )
import           System.IO.Temp
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.Process
import           Graphics.Netpbm
import qualified Data.ByteString as B

import MyPhoto.Model
import MyPhoto.Utils

calculateDistance :: [Int] -> [Int] -> Double
calculateDistance vec1 vec2 =
  sqrt .
  fromIntegral .
  sum .
  map ((\v -> v*v) . (\(v1, v2) -> v1 - v2)) $ zip vec1 vec2

computImgVecs :: FilePath -> Img -> IO (Img, [Int])
computImgVecs tmpdir img = do
  let ppmFile = tmpdir </> (takeFileName img ++ ".ppm")

  (_, _, _, pHandle) <- createProcess (proc "convert" [img, "-resize", "3x3!", ppmFile])
  exitCode <- waitForProcess pHandle
  unless (exitCode == ExitSuccess) $
    fail ("Resize failed with " ++ show exitCode)


  ppmResult <- parsePPM <$> B.readFile ppmFile
  case ppmResult of
    Right ([ppmImg],_) -> let
        imgVec = (pixelDataToIntList . ppmData) ppmImg
      in return (img, imgVec)
    Left err           -> fail ("PPM parsing failed with " ++ err)


rmOutliersImpl :: Double -> [Img] -> PActionBody
rmOutliersImpl maxDist imgs@(img1:_) = let
    dropByDistances :: [(Img, [Int])] -> [Int] -> IO [Img]
    dropByDistances [] lastVec = return []
    dropByDistances ((img, curVec):imgsWithVecs) lastVec = let
        dist = calculateDistance lastVec curVec
      in if dist < maxDist
         then do
           fmap (img :) (dropByDistances imgsWithVecs curVec)
         else do
           putStrLn ("drop " ++ img ++ " with dist " ++ (show dist))
           dropByDistances imgsWithVecs lastVec
  in do
    withTempDirectory (takeDirectory img1) "_outliers.tmp"
      (\tmpdir -> do
          imgsWithVecs <- mapM (computImgVecs tmpdir) imgs
          imgsWithoutOutliers <- dropByDistances imgsWithVecs []
          return (Right imgsWithoutOutliers)
        )

help :: PAction
help = PAction $ \_ -> pure (Left "Usage: rmoutliers [dist] files...")
rmOutliers :: PrePAction
rmOutliers ("-h":_) = help
rmOutliers []       = logSeparator "rmoutliers" <> PAction (rmOutliersImpl 10)
rmOutliers [dist]   = logSeparator "rmoutliers" <> PAction (rmOutliersImpl (read dist))
rmOutliers _        = help
