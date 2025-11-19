module MyPhoto.Actions.FocusStack
  ( focusStackImgs,
  )
where

import MyPhoto.Model
import MyPhoto.Wrapper.FocusStackWrapper
import qualified System.IO as IO
import Control.Exception (SomeException, try)
import Control.Concurrent (getNumCapabilities)
import MyPhoto.Actions.Metadata (getStackOutputBN)

backoffStrategy :: [(Maybe Int, Maybe Int)]
backoffStrategy = [(Nothing,Nothing), (Just 6, Just 14), (Just 4, Just 6)]

mkOptions :: Bool -> [String] -> [FilePath] -> IO FocusStackOptions
mkOptions verbose additionalParameters imgs = do
  outputBN <- getStackOutputBN imgs
  output <- makeAbsolute (outputBN ++ "_focus-stack.png")
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
      capabilities <- getNumCapabilities
      tryWithBackoff options capabilities backoffStrategy

tryWithBackoff :: FocusStackOptions -> Int -> [(Maybe Int, Maybe Int)] -> IO (FilePath, [FilePath])
tryWithBackoff options capabilities strategies = 
  case strategies of
    [] -> runFocusStack options
    [lastStrategy] -> do
      let (strategyOptions, strategyDesc) = applyStrategy options capabilities lastStrategy
      result <- try (runFocusStack strategyOptions) :: IO (Either SomeException (FilePath, [FilePath]))
      case result of
        Right res -> return res
        Left ex -> do
          IO.hPutStrLn IO.stderr $ "WARNING: focus stacking failed with error: " ++ show ex
          IO.hPutStrLn IO.stderr $ "INFO: try with " ++ strategyDesc
          runFocusStack strategyOptions
    (strategy : rest) -> do
      let (strategyOptions, strategyDesc) = applyStrategy options capabilities strategy
      result <- try (runFocusStack strategyOptions) :: IO (Either SomeException (FilePath, [FilePath]))
      case result of
        Right res -> return res
        Left ex -> do
          IO.hPutStrLn IO.stderr $ "WARNING: focus stacking failed with error: " ++ show ex
          IO.hPutStrLn IO.stderr $ "INFO: try with " ++ strategyDesc
          tryWithBackoff options capabilities rest

applyStrategy :: FocusStackOptions -> Int -> (Maybe Int, Maybe Int) -> (FocusStackOptions, String)
applyStrategy options capabilities (mbBatchsize, mbThreads) =
  let (threads, threadsValue) = case mbThreads of
        Just maxThreads -> 
          let capped = if capabilities > maxThreads then maxThreads else capabilities
          in (Just capped, capped)
        Nothing -> (Nothing, capabilities)
      strategyOptions = options { _batchsize = mbBatchsize, _threads = threads }
      strategyDesc = case (mbBatchsize, mbThreads) of
        (Nothing, Nothing) -> "default options"
        (Just bs, Just ts) -> "batchsize=" ++ show bs ++ " and threads=" ++ show threadsValue ++ " (capped from " ++ show ts ++ ")"
        (Just bs, Nothing) -> "batchsize=" ++ show bs ++ " and threads=" ++ show threadsValue
        (Nothing, Just ts) -> "default batchsize and threads=" ++ show threadsValue ++ " (capped from " ++ show ts ++ ")"
  in (strategyOptions, strategyDesc)
          
