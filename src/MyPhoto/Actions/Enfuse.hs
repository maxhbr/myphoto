module MyPhoto.Actions.Enfuse
  ( EnfuseOptions (..),
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

data ChunkSettings
  = ChunkParameters Int Int Int
  | ChunkSize Int
  | NoChunks
  deriving (Show, Eq)

instance Default ChunkSettings where
  def = ChunkParameters 2 6 15 -- chunkLevel minChunkSize maxChunkSize

computeChunkSize :: ChunkSettings -> Imgs -> Maybe Int
computeChunkSize (ChunkParameters chunkLevel minChunkSize maxChunkSize) imgs =
  let chunkSizeFromChunkLevel = case chunkLevel of
        2 -> (ceiling (sqrt (fromIntegral (length imgs))))
        1 -> length imgs
        _ -> undefined
      chunkSize = max (min chunkSizeFromChunkLevel maxChunkSize) minChunkSize
   in if chunkSize >= length imgs
        then Nothing
        else Just chunkSize
computeChunkSize (ChunkSize chunkSize) imgs =
  if chunkSize >= length imgs
    then Nothing
    else Just chunkSize
computeChunkSize NoChunks imgs = Nothing

data EnfuseOptions = EnfuseOptions
  { optEnfuseVerbose :: Bool,
    optMaxCapabilities :: Int,
    optOutputBN :: Maybe String,
    optChunk :: ChunkSettings,
    optProjection :: Projection,
    optOpts :: EnfuseOpts,
    optConcurrent :: Bool,
    optAll :: Bool,
    optSaveMasks :: Bool
  }
  deriving (Show)

instance Default EnfuseOptions where
  def = EnfuseOptions
    { optEnfuseVerbose = False,
      optMaxCapabilities = 9, -- with to many threads the memory seems to be insufficient
      optOutputBN = Nothing,
      optChunk = def,
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
   in "_" ++ projectionOptionsToFilenameAppendix o ++ optsOptionsToFilenameAppendix o

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
runEnfuse retries args@(outFile', workdir, saveMasks, enfuseArgs) imgs' = do
  putStrLn (">>>>>>>>>>>>>>>>>>>>>>>> start >> " ++ outFile')
  let outFile = inWorkdir workdir outFile'
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
      putStrLn ("<<<<<<<<<<<<<<<<<<<<<<<<< done << " ++ outFile')
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

getStackedFilename :: EnfuseOptions -> FilePath -> FilePath
getStackedFilename opts img =
  let (bn, ext') = splitExtensions img
      ext = if map toLower ext' == ".jpg" then ".png" else ext
   in case optOutputBN opts of
        Nothing -> bn ++ "_enfuse" ++ optionsToFilenameAppendix opts <.> ext
        Just outputBN -> outputBN ++ "_enfuse" ++ optionsToFilenameAppendix opts <.> ext

enfuseStackImgs :: EnfuseOptions -> [FilePath] -> IO (Either String [FilePath])
enfuseStackImgs =
  let stackImpl'' :: MS.MSem Int -> EnfuseOptions -> (FilePath, FilePath, Bool, [String]) -> [FilePath] -> IO (Either String [FilePath])
      stackImpl'' sem opts args@(outFile, workdir, saveMasks, enfuseArgs) imgs = do
        putStrLn ("#### calculate " ++ outFile)
        let chunkSize = computeChunkSize (optChunk opts) imgs
            maybeChunks = case chunkSize of
              Nothing -> Nothing
              Just chunkSize ->
                let joinLastTwoChunks :: [[FilePath]] -> [[FilePath]]
                    joinLastTwoChunks [] = []
                    joinLastTwoChunks [chunk] = [chunk]
                    joinLastTwoChunks [chunk1, chunk2] = [chunk1 ++ chunk2]
                    joinLastTwoChunks (chunk1 : chunk2 : chunks) = chunk1 : joinLastTwoChunks (chunk2 : chunks)
                    joinLastTwoChunksIfNeeded :: [[FilePath]] -> [[FilePath]]
                    joinLastTwoChunksIfNeeded [] = []
                    joinLastTwoChunksIfNeeded [chunk] = [chunk]
                    joinLastTwoChunksIfNeeded chunks =
                      if length (last chunks) > (chunkSize `div` 2)
                        then chunks
                        else joinLastTwoChunks chunks
                 in Just . joinLastTwoChunksIfNeeded $ chunksOf chunkSize imgs
         in case maybeChunks of
              Just chunks | length chunks > 1 -> do
                let chunkSize = length $ head chunks
                let numberOfChunks = length chunks
                putStrLn ("#### use " ++ show numberOfChunks ++ " chunks of chunkSize " ++ show chunkSize ++ " to calculate " ++ outFile)
                chunkImgs <-
                  mapConcurrently
                    ( \(i, chunk) -> do
                        let (bn, ext) = splitExtensions outFile
                            chunkOutputDir = workdir </> bn <> "_chunks_of" <> show chunkSize
                            chunkOutputFilename = bn <> "_chunk" <> show i <> "of" <> show numberOfChunks <.> ext
                        createDirectoryIfMissing True chunkOutputDir
                        stackImpl'' sem opts (chunkOutputFilename, chunkOutputDir, saveMasks, enfuseArgs) chunk
                    )
                    (zip [1 ..] chunks)

                case foldl foldResults (Right []) chunkImgs of
                  Right chunkImgs' -> stackImpl'' sem opts args chunkImgs'
                  err -> return err
              _ -> do
                avaial <- MS.peekAvail sem
                putStrLn ("#### use " ++ show (length imgs) ++ " images to calculate " ++ outFile ++ " (available threads: " ++ show avaial ++ ")")
                (MS.with sem . runEnfuse 2 args) imgs

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
                  let workdir = "."
                  stackImpl'' sem opts (outFile, workdir, optSaveMasks opts, enfuseArgs) imgs
   in \opts imgs -> do
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
