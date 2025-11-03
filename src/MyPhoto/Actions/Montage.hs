module MyPhoto.Actions.Montage
  ( montage,
    montageSample,
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
  let (bn, _) = splitExtensions outputFileBN
      outputFile = outputFileBN ++ "_MONTAGE.png"
   in do
        alreadyExists <- doesFileExist outputFile
        if alreadyExists
          then putStrLn ("previously generated " ++ outputFile)
          else do
            (_, _, _, pHandle) <-
              createProcess
                ( proc
                    "montage"
                    ( concat
                        [ ["-geometry", show xySize ++ "x" ++ show xySize ++ "+2+2"],
                          imgs,
                          [outputFile]
                        ]
                    )
                )
            exitCode <- waitForProcess pHandle
            unless (exitCode == ExitSuccess) $
              fail ("Resize failed with " ++ show exitCode)
        return outputFile

montageSample :: Int -> Int -> FilePath -> Imgs -> IO FilePath
montageSample sampleSize xySize outputFileBN imgs =
  let imgs' = sampleOfM sampleSize imgs
   in montage xySize outputFileBN imgs'
