module MyPhoto.Actions.FocusStack
  ( focusStackImgs,
  )
where

import qualified Data.Map as Map
import MyPhoto.Model
import MyPhoto.Wrapper.FocusStackWrapper
import qualified System.IO as IO
import System.Process

focusStackImgs :: Bool -> [String] -> [FilePath] -> IO (FilePath, [FilePath])
focusStackImgs verbose additionalParameters imgs = do
  output <- makeAbsolute (computeStackOutputBN imgs ++ "_focus-stack.png")
  let focusStackWorkdir = output -<.> "workdir"

  outputExists <- doesFileExist output
  if outputExists
    then do
      IO.hPutStrLn IO.stderr $ "INFO: focus-stack output " ++ output ++ " already exists"
      let alignedImgs = computeAlignedImgs focusStackWorkdir imgs
      return (output, alignedImgs)
    else do
      runFocusStack
        FocusStackOptions
          { _verbose = verbose,
            -- _cropping = FocusStackNoCrop,
            _cropping = FocusStackCroppingDefault,
            _additionalParameters = additionalParameters,
            _imgs = imgs,
            _workdir = focusStackWorkdir,
            _output = output
          }
