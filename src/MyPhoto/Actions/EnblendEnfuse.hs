module MyPhoto.Actions.EnblendEnfuse
  ( EnblendEnfuseOptions (..),
    EnblendEnfuseActionOptions (..),
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
import MyPhoto.Wrapper.EnblendEnfuse
import System.Console.GetOpt
import System.Directory
import System.Exit
import System.FilePath
import System.Process

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

data EnblendEnfuseActionOptions = EnblendEnfuseActionOptions
  { eeOptions :: EnblendEnfuseOptions,
    eeaMaxCapabilities :: Int,
    eeaOutputBN :: Maybe String,
    eeaChunk :: ChunkSettings,
    eeaConcurrent :: Bool,
    eeaAll :: Bool
  }
  deriving (Show)

instance Default EnblendEnfuseActionOptions where
  def = EnblendEnfuseActionOptions
    { eeOptions = def,
      eeaMaxCapabilities = 9, -- with to many threads the memory seems to be insufficient
      eeaOutputBN = Nothing,
      eeaChunk = def,
      eeaConcurrent = True,
      eeaAll = False
    }

optionsToFilenameAppendix :: EnblendEnfuseActionOptions -> String
optionsToFilenameAppendix EnblendEnfuseActionOptions {eeOptions = o } =
  let projectionOptionsToFilenameAppendix :: EnblendEnfuseOptions -> String
      projectionOptionsToFilenameAppendix EnblendEnfuseOptions {eeProjection = Proj1} = "p1"
      projectionOptionsToFilenameAppendix EnblendEnfuseOptions {eeProjection = Proj2} = "p2"
      projectionOptionsToFilenameAppendix EnblendEnfuseOptions {eeProjection = Proj3} = "p3"
      optsOptionsToFilenameAppendix :: EnblendEnfuseOptions -> String
      optsOptionsToFilenameAppendix EnblendEnfuseOptions {eeOpts = Opts1} = "o1"
      optsOptionsToFilenameAppendix EnblendEnfuseOptions {eeOpts = Opts2} = "o2"
      optsOptionsToFilenameAppendix EnblendEnfuseOptions {eeOpts = Opts3} = "o3"
   in "_" ++ projectionOptionsToFilenameAppendix o ++ optsOptionsToFilenameAppendix o

foldResults :: Either String [FilePath] -> Either String [FilePath] -> Either String [FilePath]
foldResults (Left err1) (Left err2) = Left (unlines [err1, err2])
foldResults r1@(Left _) _ = r1
foldResults (Right imgs1) (Right imgs2) = Right (imgs1 ++ imgs2)
foldResults _ r2@(Left _) = r2

getStackedFilename :: EnblendEnfuseActionOptions -> Imgs -> FilePath
getStackedFilename _ [] = undefined
getStackedFilename opts (img:_) =
  let (bn, ext') = splitExtensions img
      ext = if map toLower ext' == ".jpg" then ".png" else ext
   in case eeaOutputBN opts of
        Nothing -> bn ++ "_enfuse" ++ optionsToFilenameAppendix opts <.> ext
        Just outputBN -> outputBN ++ "_enfuse" ++ optionsToFilenameAppendix opts <.> ext

enfuseStackImgs :: EnblendEnfuseActionOptions -> [FilePath] -> IO (Either String [FilePath])
enfuseStackImgs =
  let stackImpl'' :: MS.MSem Int -> EnblendEnfuseActionOptions -> FilePath -> FilePath -> [FilePath] -> IO (Either String [FilePath])
      stackImpl'' sem opts outFile workdir imgs = do
        putStrLn ("#### calculate " ++ outFile)
        let chunkSize = computeChunkSize (eeaChunk opts) imgs
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
                        stackImpl'' sem opts chunkOutputFilename chunkOutputDir imgs
                    )
                    (zip [1 ..] chunks)

                case foldl foldResults (Right []) chunkImgs of
                  Right chunkImgs' -> stackImpl'' sem opts outFile workdir chunkImgs'
                  err -> return err
              _ -> do
                avaial <- MS.peekAvail sem
                putStrLn ("#### use " ++ show (length imgs) ++ " images to calculate " ++ outFile ++ " (available threads: " ++ show avaial ++ ")")
                (MS.with sem . runEnblendEnfuseWithRetries 2 (eeOptions opts) outFile workdir) imgs

      stackImpl' :: MS.MSem Int -> EnblendEnfuseActionOptions -> [FilePath] -> IO (Either String [FilePath])
      stackImpl' sem opts imgs =do
        let outFile = getStackedFilename opts imgs

        outFileExists <- doesFileExist outFile
        if outFileExists
          then do
            putStrLn ("#### " ++ outFile ++ " already exists, skipping")
            return (Right [outFile])
          else do
            let workdir = "."
            stackImpl'' sem opts outFile workdir imgs
   in \opts imgs -> do
        print opts
        numCapabilities <- getNumCapabilities
        let numThreads =
              if eeaConcurrent opts
                then min numCapabilities (eeaMaxCapabilities opts)
                else 1
        when (eeaConcurrent opts) $ putStrLn ("#### use " ++ show numThreads ++ " threads")
        sem <- MS.new numThreads -- semathore to limit number of parallel threads
        if eeaAll opts
          then do
            let eeOpts = eeOptions opts
            results <-
              mapConcurrently
                (\opts' -> stackImpl' sem opts' imgs)
                [ opts {eeaAll = False, eeOptions = eeOpts{eeProjection = Proj1, eeOpts = Opts1}},
                  opts {eeaAll = False, eeOptions = eeOpts{eeProjection = Proj1, eeOpts = Opts2}},
                  opts {eeaAll = False, eeOptions = eeOpts{eeProjection = Proj1, eeOpts = Opts3}},
                  opts {eeaAll = False, eeOptions = eeOpts{eeProjection = Proj2, eeOpts = Opts1}},
                  opts {eeaAll = False, eeOptions = eeOpts{eeProjection = Proj2, eeOpts = Opts2}},
                  opts {eeaAll = False, eeOptions = eeOpts{eeProjection = Proj2, eeOpts = Opts3}},
                  opts {eeaAll = False, eeOptions = eeOpts{eeProjection = Proj3, eeOpts = Opts1}},
                  opts {eeaAll = False, eeOptions = eeOpts{eeProjection = Proj3, eeOpts = Opts2}},
                  opts {eeaAll = False, eeOptions = eeOpts{eeProjection = Proj3, eeOpts = Opts3}}
                ]
            return (foldl foldResults (Right []) results)
          else stackImpl' sem opts imgs
