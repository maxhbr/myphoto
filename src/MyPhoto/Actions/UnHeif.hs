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
calculateUnHeifedName = (`replaceExtension` "jpg") -- jpgs are way faster then pngs ?!?

unHeifImpl1 :: Bool -> Img -> IO Img
unHeifImpl1 removeHeif img =
  let args = ["--quality", "100"]
      outImg = calculateUnHeifedName img
   in do
        exists <- doesFileExist outImg
        unless exists $ do
          putStrLn (img ++ " --> " ++ outImg)
          (_, _, _, pHandle) <- createProcess (proc "heif-convert" (args ++ [img, outImg]))
          exitCode <- waitForProcess pHandle
          unless (exitCode == ExitSuccess) $
            fail ("UnHeif failed with " ++ show exitCode)
          when removeHeif $
            removeFile img
        return outImg

unHeif :: Bool -> Imgs -> IO Imgs
unHeif removeHeif imgs = do
  sem <- MS.new numCapabilities
  mapConcurrently (MS.with sem . unHeifImpl1 removeHeif) imgs

