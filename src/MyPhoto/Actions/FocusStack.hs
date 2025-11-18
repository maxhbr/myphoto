module MyPhoto.Actions.FocusStack
  ( focusStackImgs,
  )
where

import MyPhoto.Model
import MyPhoto.Wrapper.FocusStackWrapper
import qualified System.IO as IO
import Control.Exception (SomeException, try)
import Control.Concurrent (getNumCapabilities)


mkOptions :: Bool -> [String] -> [FilePath] -> IO FocusStackOptions
mkOptions verbose additionalParameters imgs = do
  output <- makeAbsolute (computeStackOutputBN imgs ++ "_focus-stack.png")
  let focusStackWorkdir = output -<.> "workdir"
  return $ (initFocusStackOptions imgs focusStackWorkdir output)
    { _verbose = verbose,
      _additionalParameters = additionalParameters
    }

focusStackImgs :: Bool -> [String] -> [FilePath] -> IO (FilePath, [FilePath])
focusStackImgs verbose additionalParameters imgs = do
  options@FocusStackOptions{ _output = output } <- mkOptions verbose additionalParameters imgs
  outputExists <- doesFileExist output
  if outputExists
    then do
      IO.hPutStrLn IO.stderr $ "INFO: focus-stack output " ++ output ++ " already exists, check that all aligned images are present"
      computeResultAndCheck options
    else do
      result <- try (runFocusStack options) :: IO (Either SomeException (FilePath, [FilePath]))
      case result of
        Right res -> return res
        Left ex -> do
          IO.hPutStrLn IO.stderr $ "WARNING: focus stacking failed with error: " ++ show ex
          capabilities <- getNumCapabilities 
          let threads = if capabilities > 14 then 14 else capabilities
          IO.hPutStrLn IO.stderr $ "INFO: try with batchsize=8 and threads=" ++ show threads
          let fallbackOptions = options { _batchsize = Just 6, _threads = Just threads }
          result' <- try (runFocusStack fallbackOptions) :: IO (Either SomeException (FilePath, [FilePath]))
          case result' of
            Right res -> return res
            Left ex' -> do
              IO.hPutStrLn IO.stderr $ "WARNING: focus stacking failed with error: " ++ show ex'
              let threads' = if capabilities > 6 then 6 else capabilities
              IO.hPutStrLn IO.stderr $ "INFO: try with batchsize=4 and threads=" ++ show threads'
              let fallbackOptions' = options { _batchsize = Just 4, _threads = Just threads' }
              runFocusStack fallbackOptions'
          
