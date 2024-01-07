module MyPhoto.Actions.FocusStack
  ( focusStackImgs,
  )
where

import qualified Data.Map as Map
import MyPhoto.Model
import qualified System.IO as IO
import System.Process

focusStackImgs :: Bool -> [String] -> [FilePath] -> IO (FilePath, [FilePath])
focusStackImgs verbose additionalParameters imgs = do
  let outputName = computeStackOutputBN imgs ++ "_focus-stack.png"
  let focusStackWorkdir = outputName -<.> "workdir"
  let alignedImgs = map (\img -> focusStackWorkdir </> "aligned_" ++ takeFileName img) imgs
  let parameters =
        ["--verbose" | verbose]
          ++ [ ("--output=../" ++ outputName),
               -- ("--depthmap=" ++ outputName ++ ".depthmap.png"),
               -- ("--3dview=" ++ outputName ++ ".3dviewpt.png"),
               "--save-steps",
               "--jpgquality=100",
               "--nocrop"
               -- , "--align-keep-size"
             ]
          ++ additionalParameters

  outputExists <- doesFileExist outputName
  if outputExists
    then do
      IO.hPutStrLn IO.stderr $ "INFO: focus-stack output " ++ outputName ++ " already exists"
    else do
      createDirectoryIfMissing True focusStackWorkdir
      putStrLn (unwords ["$ focus-stack", unwords parameters, "[img [img [...]]]"])
      (_, _, _, ph) <-
        createProcess
          ( proc
              "focus-stack"
              (parameters ++ imgs)
          )
            { cwd = Just focusStackWorkdir
            }
      exitcode <- waitForProcess ph
      if exitcode /= ExitSuccess
        then do
          fail $ "ERR: focus-stack exited with " ++ (show exitcode)
        else do
          return ()
      exists <- doesFileExist outputName
      unless exists $ do
        fail $ "image not found: " ++ outputName
  mapM_
    ( \img -> do
        exists <- doesFileExist img
        unless exists $ do
          fail $ "image not found: " ++ img
    )
    alignedImgs

  return (outputName, alignedImgs)
