module MyPhoto.Actions.Downscale
  ( downscaleImgs,
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

downscaleImg :: Int -> FilePath -> Img -> IO Img
downscaleImg int workdir img = do
  let (bn, ext) = splitExtensions (takeFileName img)
      outputImg = inWorkdir workdir (bn ++ "@" ++ show int ++ ext)
      pngArgs = if ext == ".png" then ["-define", "png:compression-level=9", "-quality", "100"] else []
      jpgArgs = if ext `elem` [".jpg", ".jpeg", ".JPG"] then ["-quality", "95"] else []
      args = ["-filter", "Lanczos", "-resize", show int ++ "%"] ++ pngArgs ++ jpgArgs
  logDebugIO $ "Downscaling " ++ img ++ " to " ++ outputImg
  (_, _, _, pHandle) <- createProcess (proc "magick" (img : args ++ [outputImg]))
  exitCode <- waitForProcess pHandle
  unless (exitCode == ExitSuccess) $
    fail ("Downscaling failed with " ++ show exitCode)
  return outputImg

downscaleImgs :: Int -> FilePath -> Imgs -> IO Imgs
downscaleImgs 100 _ imgs = return imgs
downscaleImgs pct workdir imgs = do
  if pct <= 0 || pct >= 100
    then fail "Downscale percentage must be between 1 and 99"
    else do
      capabilities <- getNumCapabilities
      let actualCapabilities = if capabilities > 4 then capabilities - 4 else capabilities
      putStrLn $ "Using " ++ show actualCapabilities ++ " (of " ++ show capabilities ++ ") concurrent threads for downscaling"
      sem <- MS.new actualCapabilities
      mapConcurrently (MS.with sem . downscaleImg pct workdir) imgs
