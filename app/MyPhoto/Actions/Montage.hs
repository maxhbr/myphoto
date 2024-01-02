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

montage :: Int -> FilePath -> Imgs -> IO FilePath
montage xySize outputFileBN imgs =
  let (bn, ext) = splitExtensions outputFileBN
      outputFile = outputFileBN ++ "_MONTAGE.png"
   in do
        (_, _, _, pHandle) <- 
          createProcess (proc "montage" 
                              (concat [ ["-geometry", show xySize ++ "x" ++ show xySize ++ "+2+2"]
                                      , imgs
                                      , [outputFile]
                                      ]))
        exitCode <- waitForProcess pHandle
        unless (exitCode == ExitSuccess) $
          fail ("Resize failed with " ++ show exitCode)
        return outputFile
