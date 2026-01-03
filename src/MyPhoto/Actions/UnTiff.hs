module MyPhoto.Actions.UnTiff
  ( unTiff,
    untiffExtensions,
  )
where

import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.MSem as MS
import Control.Monad
import GHC.Conc (numCapabilities)
import MyPhoto.Model
import System.Directory
import System.FilePath
import System.Process

untiffExtensions :: [String]
untiffExtensions = [".tiff", ".tif"]

calculateUntiffedName :: Img -> Img
calculateUntiffedName = (`replaceExtension` "png")

unTiffImpl1 :: Img -> IO (Img, Img)
unTiffImpl1 img =
  let args =
        [ "-depth",
          "24",
          "-define",
          "png:compression-filter=2",
          "-define",
          "png:compression-level=9",
          "-define",
          "png:compression-strategy=1"
        ]
      png = calculateUntiffedName img
   in do
        exists <- doesFileExist png
        unless exists $ do
          logInfoIO (img ++ " --> " ++ png)
          (_, _, _, pHandle) <- createProcess (proc "magick" (args ++ [img, png]))
          exitCode <- waitForProcess pHandle
          unless (exitCode == ExitSuccess) $
            fail ("UnTiff failed with " ++ show exitCode)
        return (img, png)

unTiff :: Bool -> Imgs -> IO Imgs
unTiff removeTiff imgs = do
  sem <- MS.new numCapabilities
  results <- mapConcurrently (MS.with sem . unTiffImpl1) imgs
  when removeTiff $
    forM_ results $
      \(img, _) -> removeFile img
  return (map snd results)
