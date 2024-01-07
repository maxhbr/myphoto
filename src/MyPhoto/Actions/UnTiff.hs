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
import System.Exit
import System.FilePath
import System.Process

untiffExtensions :: [String]
untiffExtensions = [".tiff", ".tif"]

calculateUntiffedName :: Img -> Img
calculateUntiffedName = (`replaceExtension` "png")

unTiffImpl1 :: Bool -> Img -> IO Img
unTiffImpl1 removeTiff img =
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
        putStrLn (img ++ " --> " ++ png)
        (_, _, _, pHandle) <- createProcess (proc "convert" (args ++ [img, png]))
        exitCode <- waitForProcess pHandle
        unless (exitCode == ExitSuccess) $
          fail ("UnTiff failed with " ++ show exitCode)
        when removeTiff $
          removeFile img
        return png

unTiff :: Bool -> Imgs -> IO Imgs
unTiff removeTiff imgs = do
  sem <- MS.new numCapabilities
  mapConcurrently (MS.with sem . unTiffImpl1 removeTiff) imgs
