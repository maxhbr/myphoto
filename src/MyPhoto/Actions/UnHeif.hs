module MyPhoto.Actions.UnHeif
  ( unHeif,
    unHeifExtensions,
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

unHeifExtensions :: [String]
unHeifExtensions = [".heif", ".heic", ".HIF", ".HEIF", ".HEIC"]

calculateUnHeifedName :: Img -> Img
calculateUnHeifedName = (`replaceExtension` "png")

unHeifImpl1 :: Bool -> Img -> IO Img
unHeifImpl1 removeHeif img =
  let args = ["--quality", "100"]
      png = calculateUnHeifedName img
   in do
        putStrLn (img ++ " --> " ++ png)
        (_, _, _, pHandle) <- createProcess (proc "heif-convert" (args ++ [img, png]))
        exitCode <- waitForProcess pHandle
        unless (exitCode == ExitSuccess) $
          fail ("UnHeif failed with " ++ show exitCode)
        when removeHeif $
          removeFile img
        return png

unHeif :: Bool -> Imgs -> IO Imgs
unHeif removeHeif imgs = do
  sem <- MS.new numCapabilities
  mapConcurrently (MS.with sem . unHeifImpl1 removeHeif) imgs

