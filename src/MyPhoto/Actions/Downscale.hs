module MyPhoto.Actions.Downscale
  ( downscaleImgs,
  )
where

import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.MSem as MS
import Control.Monad
import MyPhoto.Model
import System.Process
import System.ProgressBar (Progress (..), defStyle, incProgress, newProgressBar)

downscaleImg :: Int -> Img -> IO Img
downscaleImg int img = do
  let (bn, _) = splitExtensions img
      outputImg = bn ++ "@" ++ show int ++ "pct.png"
  outImgExists <- doesFileExist outputImg
  if outImgExists
    then do
      return outputImg
    else do
      let pngArgs = ["-define", "png:compression-level=9", "-quality", "100"]
          args = ["-filter", "Lanczos", "-resize", show int ++ "%"] ++ pngArgs
      (_, _, _, pHandle) <- createProcess (proc "magick" (img : args ++ [outputImg]))
      exitCode <- waitForProcess pHandle
      unless (exitCode == ExitSuccess) $
        fail ("Downscaling failed with " ++ show exitCode)
      return outputImg

downscaleImgs :: Int -> Imgs -> IO Imgs
downscaleImgs 100 imgs = return imgs
downscaleImgs pct imgs = do
  if pct <= 0 || pct >= 100
    then fail "Downscale percentage must be between 1 and 99"
    else do
      capabilities <- getNumCapabilities
      let actualCapabilities = if capabilities > 4 then capabilities - 4 else capabilities
      putStrLn $ "Using " ++ show actualCapabilities ++ " (of " ++ show capabilities ++ ") concurrent threads for downscaling"
      pb <- newProgressBar defStyle 10 (Progress 0 (length imgs) ())
      sem <- MS.new actualCapabilities
      mapConcurrently
        ( ( \img -> MS.with sem $ do
              img' <- downscaleImg pct img
              incProgress pb 1
              return img'
          )
        )
        imgs
