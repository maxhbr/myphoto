module MyPhoto.Video where

import System.Process
import MyPhoto.Model

extractFrames :: FilePath -> IO [FilePath]
extractFrames vid = let
    (dir, bn) = splitFileName vid
    imgDir = dir </> bn <.> "frames"
  in do
    createDirectoryIfMissing True imgDir
    (_, _, _, pHandle) <-
      createProcess
        ( proc
            "ffmpeg"
            ( concat
                [ ["-i", vid],
                  [imgDir </> bn ++ "_%04d.png"]
                ]
            )
        )
    exitCode <- waitForProcess pHandle
    unless (exitCode == ExitSuccess) $
      fail ("Extract of images failed with " ++ show exitCode)
    map (imgDir </>) <$> listDirectory imgDir 
