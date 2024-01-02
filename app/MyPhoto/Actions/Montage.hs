module MyPhoto.Actions.Montage
  ( montage,
  )
where

import Control.Monad
import MyPhoto.Model
import System.Directory
import System.Exit
import System.FilePath
import System.Process

montage :: FilePath -> Imgs -> IO FilePath
montage outputFileBN imgs =
  let (bn, ext) = splitExtensions outputFileBN
      outputFile = outputFileBN ++ "_MONTAGE.png"
   in do
        (_, _, _, pHandle) <- createProcess (proc "montage" (["-geometry", "100x100+2+2"] ++ imgs ++ [outputFile]))
        exitCode <- waitForProcess pHandle
        unless (exitCode == ExitSuccess) $
          fail ("Resize failed with " ++ show exitCode)
        return outputFile
