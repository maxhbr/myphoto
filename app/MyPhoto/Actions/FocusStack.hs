module MyPhoto.Actions.FocusStack
  ( focusStackImgs,
  )
where

import MyPhoto.Model
import qualified System.IO as IO
import qualified Data.Map as Map
import System.Process

focusStackImgs :: Options -> [FilePath] -> IO (FilePath, [FilePath])
focusStackImgs opts imgs = do
  let outputName = computeStackOutputBN imgs ++ "_focus-stack.png"
  let focusStackWorkdir = outputName -<.> "workdir"
  let alignedImgs = map (\img -> focusStackWorkdir </> "aligned_" ++ takeFileName img) imgs
  let outputs = outputName : alignedImgs

  let additionalParameters = Map.findWithDefault [] "focus-stack" (optParameters opts)
  let parameters = [ ("--output=../" ++ outputName),
                     ("--depthmap=" ++ outputName ++ ".depthmap.png"),
                     ("--3dview=" ++ outputName ++ ".3dviewpt.png"),
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
              "focus-stack" ( parameters ++ imgs)
          )
            { cwd = Just focusStackWorkdir
            }
      exitcode <- waitForProcess ph
      IO.hPutStrLn IO.stderr $ "INFO: focus-stack exited with " ++ (show exitcode)

  return (outputName, alignedImgs)
