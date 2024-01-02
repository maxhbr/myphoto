module MyPhoto.Actions.Enfuse
  ( EnfuseOptions (..),
    enfuseDefaultOptions,
    enfuseStackImgs,
  )
where

import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.MSem as MS
import Control.Monad
import Data.Char (toLower)
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe)
import MyPhoto.Model hiding (Options (..))
import System.Console.GetOpt
import System.Directory
import System.Exit
import System.FilePath
import System.Process

data Projection
  = Proj1
  | Proj2
  | Proj3
  deriving (Show)

projectionToArgs :: Projection -> [String]
projectionToArgs Proj1 = ["--gray-projector=l-star"]
projectionToArgs Proj2 = []
projectionToArgs Proj3 = ["--gray-projector=luminance"]

data EnfuseOpts
  = Opts1
  | Opts2
  | Opts3
  deriving (Show)

optsToArgs :: EnfuseOpts -> [String]
optsToArgs Opts1 = ["--contrast-window-size=5"]
optsToArgs Opts2 = ["--contrast-edge-scale=0.3"]
optsToArgs Opts3 = ["--contrast-edge-scale=31", "--contrast-min-curvature=11"]

data EnfuseOptions = EnfuseOptions
  { optEnfuseVerbose :: Bool,
    optMaxCapabilities :: Int,
    optOutputBN :: Maybe String,
    optChunkMaxSize :: Int,
    optChunkLevel :: Int,
    optChunkSize :: Maybe Int,
    optProjection :: Projection,
    optOpts :: EnfuseOpts,
    optConcurrent :: Bool,
    optAll :: Bool,
    optSaveMasks :: Bool
  }
  deriving (Show)

enfuseDefaultOptions :: EnfuseOptions
enfuseDefaultOptions =
  EnfuseOptions
    { optEnfuseVerbose = False,
      optMaxCapabilities = 9, -- with to many threads the memory seems to be insufficient
      optOutputBN = Nothing,
      optChunkMaxSize = 15,
      optChunkLevel = 2,
      optChunkSize = Nothing,
      optProjection = Proj1,
      optOpts = Opts1,
      optConcurrent = True,
      optAll = False,
      optSaveMasks = False
    }

optionsToFilenameAppendix :: EnfuseOptions -> String
optionsToFilenameAppendix o =
  let projectionOptionsToFilenameAppendix :: EnfuseOptions -> String
      projectionOptionsToFilenameAppendix EnfuseOptions {optProjection = Proj1} = "p1"
      projectionOptionsToFilenameAppendix EnfuseOptions {optProjection = Proj2} = "p2"
      projectionOptionsToFilenameAppendix EnfuseOptions {optProjection = Proj3} = "p3"
      optsOptionsToFilenameAppendix :: EnfuseOptions -> String
      optsOptionsToFilenameAppendix EnfuseOptions {optOpts = Opts1} = "o1"
      optsOptionsToFilenameAppendix EnfuseOptions {optOpts = Opts2} = "o2"
      optsOptionsToFilenameAppendix EnfuseOptions {optOpts = Opts3} = "o3"
      chunkOptionsToFilenameAppendix EnfuseOptions {optChunkSize = Nothing} = ""
      chunkOptionsToFilenameAppendix EnfuseOptions {optChunkSize = Just n} = "c" ++ show n
   in "_" ++ projectionOptionsToFilenameAppendix o ++ optsOptionsToFilenameAppendix o ++ chunkOptionsToFilenameAppendix o

getEnfuseArgs :: EnfuseOptions -> [String]
getEnfuseArgs opts =
  let focusStackArgs = ["--exposure-weight=0", "--saturation-weight=0", "--contrast-weight=1"]
      hardMaskArgs = ["--hard-mask"]
      verbosityArgs = ["-v" | optEnfuseVerbose opts]
   in verbosityArgs
        ++ focusStackArgs
        ++ hardMaskArgs
        ++ projectionToArgs (optProjection opts)
        ++ optsToArgs (optOpts opts)

runEnfuse :: Int -> (FilePath, FilePath, Bool, [String]) -> [FilePath] -> IO (Either String [FilePath])
runEnfuse _ _ [img] = return (Right [img])
runEnfuse retries args@(outFile, workdir, saveMasks, enfuseArgs) imgs' = do
  putStrLn (">>>>>>>>>>>>>>>>>>>>>>>> start >> " ++ outFile)
  let outMasksFolder = inWorkdir workdir (outFile ++ "-masks")
  when saveMasks $
    createDirectoryIfMissing True outMasksFolder
  let maskArgs = ["--save-masks=\"" ++ outMasksFolder ++ "/softmask-%04n.tif:" ++ outMasksFolder ++ "/hardmask-%04n.tif\"" | saveMasks]
      outputArgs = ["--output=" ++ outFile]
  putStrLn (unwords ["$ enfuse", unwords (enfuseArgs ++ maskArgs ++ outputArgs), "[img [img [...]]]"])
  (_, _, _, pHandle) <-
    createProcess
      ( proc
          "enfuse"
          ( enfuseArgs
              ++ maskArgs
              ++ outputArgs
              ++ imgs'
          )
      )
  exitCode <- waitForProcess pHandle

  case exitCode of
    ExitSuccess -> do
      putStrLn ("<<<<<<<<<<<<<<<<<<<<<<<<< done << " ++ outFile)
      return (Right [outFile])
    _ -> do
      let msg = "Stack of " ++ outFile ++ " failed with " ++ show exitCode
      if retries > 0
        then do
          putStrLn ("### " ++ msg ++ " (retrying)")
          runEnfuse (retries - 1) args imgs'
        else do
          putStrLn ("### " ++ msg ++ " (giving up)")
          return (Left msg)

foldResults :: Either String [FilePath] -> Either String [FilePath] -> Either String [FilePath]
foldResults (Left err1) (Left err2) = Left (unlines [err1, err2])
foldResults r1@(Left _) _ = r1
foldResults (Right imgs1) (Right imgs2) = Right (imgs1 ++ imgs2)
foldResults _ r2@(Left _) = r2

{--
 - if nothing is return, that means that no further splitting is necessary
 - otherwise the size of the next chunks is returned
 -}
calculateNextChunkSize :: EnfuseOptions -> [FilePath] -> Maybe Int
calculateNextChunkSize opts imgs =
  let numOfImages = length imgs
   in case optChunkSize opts of
        Nothing -> Nothing
        Just maxChunkSize
          | numOfImages <= 2 ->
              Nothing
          | numOfImages <= maxChunkSize ->
              Nothing
          | numOfImages <= maxChunkSize * maxChunkSize ->
              Just (maximum [numOfImages `div` (ceiling ((fromIntegral numOfImages) / (fromIntegral maxChunkSize))), 2])
          | numOfImages > maxChunkSize * maxChunkSize ->
              Just (numOfImages `div` maxChunkSize)
        Just _ -> Nothing

getStackedFilename :: EnfuseOptions -> FilePath -> FilePath
getStackedFilename opts img =
  let (bn, ext') = splitExtensions img
      ext = if map toLower ext' == ".jpg" then ".png" else ext
   in case optOutputBN opts of
        Nothing -> bn ++ "_enfuse" ++ optionsToFilenameAppendix opts <.> ext
        Just outputBN -> outputBN ++ "__enfuse" ++ optionsToFilenameAppendix opts <.> ext

getChunkFilename :: FilePath -> FilePath -> Int -> Int -> FilePath
getChunkFilename workdir img indexOfChunk numberOfChunks =
  let (bn, ext') = splitExtensions img
      ext = if map toLower ext' == ".jpg" then ".png" else ext
   in inWorkdir workdir (bn ++ "_chunk" ++ show indexOfChunk ++ "of" ++ show numberOfChunks <.> ext)

enfuseStackImgs :: EnfuseOptions -> [FilePath] -> IO (Either String [FilePath])
enfuseStackImgs opts' =
  let stackImpl'' :: MS.MSem Int -> EnfuseOptions -> (FilePath, FilePath, Bool, [String]) -> [FilePath] -> IO (Either String [FilePath])
      stackImpl'' sem opts args@(outFile, workdir, saveMasks, enfuseArgs) imgs = case calculateNextChunkSize opts imgs of
        Nothing -> do
          (MS.with sem . runEnfuse 2 args) imgs
        Just maxChunkSize ->
          let chunks = chunksOf maxChunkSize imgs
              numberOfChunks = length chunks
           in do
                putStrLn ("#### use " ++ show numberOfChunks ++ " chunks of maxChunkSize " ++ show maxChunkSize ++ " to calculate " ++ outFile)

                chunkImgs <-
                  mapConcurrently
                    ( \(i, chunk) -> do
                        let chunkOutputFilename = getChunkFilename workdir outFile i numberOfChunks
                        stackImpl'' sem opts (chunkOutputFilename, workdir, saveMasks, enfuseArgs) chunk
                    )
                    (zip [1 ..] chunks)

                case foldl foldResults (Right []) chunkImgs of
                  Right chunkImgs' -> stackImpl'' sem opts args chunkImgs'
                  err -> return err

      stackImpl' :: MS.MSem Int -> EnfuseOptions -> [FilePath] -> IO (Either String [FilePath])
      stackImpl' sem opts imgs =
        let enfuseArgs = getEnfuseArgs opts
         in do
              let outFile = getStackedFilename opts (head imgs)

              outFileExists <- doesFileExist outFile
              if outFileExists
                then do
                  putStrLn ("#### " ++ outFile ++ " already exists, skipping")
                  return (Right [outFile])
                else do
                  let workdir = outFile -<.> "workdir"
                  createDirectoryIfMissing True workdir
                  stackImpl'' sem opts (outFile, workdir, optSaveMasks opts, enfuseArgs) imgs
   in \imgs -> do
        -- apply autochunk
        let opts = case optChunkSize opts' of
              Nothing ->
                let chunkSizeFromChunkLevel = case optChunkLevel opts' of
                      2 -> (ceiling (sqrt (fromIntegral (length imgs))))
                      1 -> length imgs
                      _ -> undefined
                    chunkSizeFromChunkMaxSize = optChunkMaxSize opts'
                    minChunkSize = max 6 (length imgs)
                    chunkSize = max (min chunkSizeFromChunkLevel chunkSizeFromChunkMaxSize) minChunkSize
                 in if chunkSize >= length imgs
                      then opts'
                      else opts' {optChunkSize = Just chunkSize}
              _ -> opts'

        print opts
        numCapabilities <- getNumCapabilities
        let numThreads =
              if optConcurrent opts
                then min numCapabilities (optMaxCapabilities opts)
                else 1
        when (optConcurrent opts) $ putStrLn ("#### use " ++ show numThreads ++ " threads")
        sem <- MS.new numThreads -- semathore to limit number of parallel threads
        if optAll opts
          then do
            results <-
              mapConcurrently
                (\opts' -> stackImpl' sem opts' imgs)
                [ opts {optAll = False, optProjection = Proj1, optOpts = Opts1},
                  opts {optAll = False, optProjection = Proj1, optOpts = Opts2},
                  opts {optAll = False, optProjection = Proj1, optOpts = Opts3},
                  opts {optAll = False, optProjection = Proj2, optOpts = Opts1},
                  opts {optAll = False, optProjection = Proj2, optOpts = Opts2},
                  opts {optAll = False, optProjection = Proj2, optOpts = Opts3},
                  opts {optAll = False, optProjection = Proj3, optOpts = Opts1},
                  opts {optAll = False, optProjection = Proj3, optOpts = Opts2},
                  opts {optAll = False, optProjection = Proj3, optOpts = Opts3}
                ]
            return (foldl foldResults (Right []) results)
          else stackImpl' sem opts imgs
