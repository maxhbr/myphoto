module MyPhoto.Actions.FocusStack
  ( focusStackImgs,
  )
where

import Control.Concurrent (getNumCapabilities)
import Control.Exception (SomeException, throwIO, try)
import Data.List (intercalate)
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import MyPhoto.Actions.Metadata (getStackOutputBN)
import MyPhoto.Model
import MyPhoto.Wrapper.FocusStackWrapper
import System.Directory (getHomeDirectory)
import qualified System.IO as IO

backoffStrategy :: [(Maybe Int, Maybe Int)]
backoffStrategy = [(Nothing, Nothing), (Just 6, Just 14), (Just 4, Just 6), (Just 3, Just 3)]

mkOptions :: Bool -> [String] -> [FilePath] -> IO FocusStackOptions
mkOptions verbose additionalParameters imgs = do
  outputBN <- getStackOutputBN imgs
  output <- makeAbsolute (outputBN ++ "_focus-stack.png")
  let focusStackWorkdir = output -<.> "workdir"
  return $
    (initFocusStackOptions imgs focusStackWorkdir output)
      { _verbose = verbose,
        _additionalParameters = additionalParameters
      }

focusStackImgs :: Bool -> [String] -> [FilePath] -> IO (FilePath, [FilePath])
focusStackImgs verbose additionalParameters imgs = do
  options@FocusStackOptions {_output = output} <- mkOptions verbose additionalParameters imgs
  outputExists <- doesFileExist output
  if outputExists
    then do
      logInfoIO ("focus-stack output " ++ output ++ " already exists, check that all aligned images are present")
      computeResultAndCheck options
    else do
      capabilities <- getNumCapabilities
      startTime <- getCurrentTime
      tryWithBackoff startTime options capabilities backoffStrategy

tryWithBackoff :: UTCTime -> FocusStackOptions -> Int -> [(Maybe Int, Maybe Int)] -> IO (FilePath, [FilePath])
tryWithBackoff startTime options capabilities strategies =
  case strategies of
    [] -> do
      result <- try (runFocusStack options) :: IO (Either SomeException (FilePath, [FilePath]))
      logFocusStackAttempt options capabilities result
      either throwIO return result
    (strategy : rest) -> do
      let (strategyOptions, strategyDesc) = applyStrategy options capabilities strategy
      logInfoIO ("try with " ++ strategyDesc)
      result <- try (runFocusStack strategyOptions) :: IO (Either SomeException (FilePath, [FilePath]))
      logFocusStackAttempt strategyOptions capabilities result
      case result of
        Right res -> return res
        Left ex -> do
          currentTime <- getCurrentTime
          logWarnIO "!!!"
          logWarnIO ("!!! focus stacking failed with error: " ++ show ex)
          logWarnIO ("!!! after " ++ show (diffUTCTime currentTime startTime) ++ " seconds")
          logWarnIO "!!!"
          if null rest
            then throwIO ex
            else do
              logInfoIO "retrying with different strategy"
              tryWithBackoff startTime options capabilities rest

applyStrategy :: FocusStackOptions -> Int -> (Maybe Int, Maybe Int) -> (FocusStackOptions, String)
applyStrategy options capabilities (mbBatchsize, mbThreads) =
  let (threads, threadsValue) = case mbThreads of
        Just maxThreads ->
          let capped = if capabilities > maxThreads then maxThreads else capabilities
           in (Just capped, capped)
        Nothing -> (Nothing, capabilities)
      strategyOptions = options {_batchsize = mbBatchsize, _threads = threads}
      strategyDesc = case (mbBatchsize, mbThreads) of
        (Nothing, Nothing) -> "default options"
        (Just bs, Just ts) -> "batchsize=" ++ show bs ++ " and threads=" ++ show threadsValue ++ " (capped from " ++ show ts ++ ")"
        (Just bs, Nothing) -> "batchsize=" ++ show bs ++ " and threads=" ++ show threadsValue
        (Nothing, Just ts) -> "default batchsize and threads=" ++ show threadsValue ++ " (capped from " ++ show ts ++ ")"
   in (strategyOptions, strategyDesc)

focusStackStatsFile :: IO FilePath
focusStackStatsFile = do
  home <- getHomeDirectory
  let dir = home </> ".myphoto"
  createDirectoryIfMissing True dir
  return $ dir </> "FocusStackStats.csv"

appendFocusStackStat :: Int -> Maybe Int -> Int -> String -> FilePath -> IO ()
appendFocusStackStat numberOfImgs batchsize usedThreads successOrFailure output = do
  statsFile <- focusStackStatsFile
  timestamp <- getCurrentTime
  let ts = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" timestamp
      batchsizeStr = maybe "default" show batchsize
      row =
        intercalate
          ","
          [show numberOfImgs, batchsizeStr, show usedThreads, successOrFailure, output, ts]
          ++ "\n"
  statsFileExists <- doesFileExist statsFile
  unless statsFileExists $ do
    IO.appendFile statsFile "number_of_imgs,batchsize,threads,success_or_failure,output,timestamp\n"
  IO.appendFile statsFile row

logFocusStackAttempt :: FocusStackOptions -> Int -> Either SomeException (FilePath, [FilePath]) -> IO ()
logFocusStackAttempt FocusStackOptions {_imgs = imgs, _threads = threads, _batchsize = batchsize, _output = output} capabilities result =
  let numberOfImgs = length imgs
      usedBatchsize = batchsize
      usedThreads = maybe capabilities id threads
      successOrFailure = either (const "failure") (const "success") result
   in appendFocusStackStat numberOfImgs usedBatchsize usedThreads successOrFailure output
