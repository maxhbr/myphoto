module MyPhoto.Utils.Chunking where

import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.MSem as MS
import Data.List.Split (chunksOf)
import MyPhoto.Model
import MyPhoto.Utils.ProgressBar (Progress (..), ProgressBar, incProgress, newProgressBarDefault)
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc, waitForProcess)

data ChunkingResult a = ChunkingResult a [ChunkingResult a]
  deriving (Show, Eq)

getCurrentsFromChunkingResults :: [ChunkingResult a] -> [a]
getCurrentsFromChunkingResults = map (\(ChunkingResult a _) -> a)

data Chunks a
  = Chunk [ChunkingResult a]
  | Chunks [Chunks a]
  deriving (Show, Eq)

toChunk :: [a] -> Chunks a
toChunk as = Chunk (map (`ChunkingResult` []) as)

countChunks :: Chunks a -> Int
countChunks (Chunk _) = 1
countChunks (Chunks chunks) = 1 + sum (map countChunks chunks)

showChunkTree :: Chunks a -> String
showChunkTree (Chunk imgs) = show (length imgs)
showChunkTree (Chunks chunks) = show (length chunks) ++ " [" ++ (unwords (map showChunkTree chunks)) ++ "]"

linearizeChunkingResults :: [ChunkingResult a] -> [a]
linearizeChunkingResults [] = []
linearizeChunkingResults (ChunkingResult a subResults : rest) =
  a : (linearizeChunkingResults subResults ++ linearizeChunkingResults rest)

linearizeChunks :: Chunks a -> [a]
linearizeChunks (Chunk crs) = linearizeChunkingResults crs
linearizeChunks (Chunks chunks) = concatMap linearizeChunks chunks

resolveChunksCollecting' :: ProgressBar () -> MS.MSem Int -> (FilePath -> [a] -> IO (Either String a)) -> FilePath -> Chunks a -> IO (Either String (ChunkingResult a))
resolveChunksCollecting' pb sem f bn (Chunk inputs) = MS.with sem $ do
  let currents = getCurrentsFromChunkingResults inputs
  ret <- f bn currents
  incProgress pb 1
  return $ case ret of
    Right a -> Right (ChunkingResult a inputs)
    Left err -> Left err
resolveChunksCollecting' pb sem f bn (Chunks chunks) = do
  let chunkSize = length chunks
  let chunkBn = \i -> bn ++ "_chunk" ++ show i ++ "of" ++ show chunkSize
  results <- mapConcurrently (\(i, c) -> resolveChunksCollecting' pb sem f (chunkBn i) c) (zip [1 ..] chunks)
  let foldResults :: Either String [ChunkingResult a] -> Either String (ChunkingResult a) -> Either String [ChunkingResult a]
      foldResults (Left err1) (Left err2) = Left (unlines [err1, err2])
      foldResults r1@(Left _) _ = r1
      foldResults (Right crs) (Right cr) = Right (crs ++ [cr])
      foldResults _ (Left e) = Left e
  let result = foldl foldResults (Right []) results
  case result of
    Right crs -> resolveChunksCollecting' pb sem f bn (Chunk crs)
    Left err -> return (Left err)

resolveChunks :: MS.MSem Int -> (FilePath -> [a] -> IO (Either String a)) -> FilePath -> Chunks a -> IO (Either String a)
resolveChunks sem f bn chunks = do
  pb <- newProgressBarDefault (Progress 0 (countChunks chunks) ())
  result <- resolveChunksCollecting' pb sem f bn chunks
  case result of
    Right crs@(ChunkingResult final _) -> do
      -- let intermediates = linearizeChunkingResults [crs]
      -- when (length intermediates >= 2) $
      --   saveLayersTiff layersTiffPath intermediates
      return (Right final)
    Left err -> return (Left err)

saveLayersTiff :: FilePath -> Imgs -> IO ()
saveLayersTiff outputTiff imgs = do
  let args = imgs ++ ["-compress", "LZW", outputTiff]
  logInfoIO $ "creating chunk layers TIFF: " ++ outputTiff
  (_, _, _, pHandle) <- createProcess (proc "magick" args) {std_out = CreatePipe, std_err = CreatePipe}
  exitCode <- waitForProcess pHandle
  unless (exitCode == ExitSuccess) $
    logWarnIO ("creating chunk layers TIFF failed with " ++ show exitCode)

joinLastTwoChunksIfNeeded :: Int -> [[a]] -> [[a]]
joinLastTwoChunksIfNeeded _ [] = []
joinLastTwoChunksIfNeeded _ [chunk] = [chunk]
joinLastTwoChunksIfNeeded chunkSize chunks =
  let joinLastTwoChunks :: [[a]] -> [[a]]
      joinLastTwoChunks [] = []
      joinLastTwoChunks [chunk] = [chunk]
      joinLastTwoChunks [chunk1, chunk2] = [chunk1 ++ chunk2]
      joinLastTwoChunks (chunk1 : chunk2 : chunks) = chunk1 : joinLastTwoChunks (chunk2 : chunks)
   in if length (last chunks) > (chunkSize `div` 2)
        then chunks
        else joinLastTwoChunks chunks

mkSparseBuckets' :: Int -> Int -> [a] -> [[a]] -> [[a]]
mkSparseBuckets' _ _ [] acc = acc
mkSparseBuckets' runningIndex bucketCount imgs@(img : imgs') acc =
  let bucket = runningIndex `mod` bucketCount
   in if length acc <= bucket
        then mkSparseBuckets' (runningIndex + 1) bucketCount imgs' (acc ++ [[img]])
        else
          let (before, curBucket : after) = splitAt bucket acc
           in mkSparseBuckets' (runningIndex + 1) bucketCount imgs' (before ++ [(curBucket ++ [img])] ++ after)

mkSparseBuckets :: Int -> [a] -> [[ChunkingResult a]]
mkSparseBuckets chunkSize imgs =
  let bucketCount = (length imgs + chunkSize - 1) `div` chunkSize
   in map (map (`ChunkingResult` [])) $
        if chunkSize >= length imgs
          then [imgs]
          else mkSparseBuckets' 0 bucketCount imgs []

mkChunks' :: Int -> [ChunkingResult a] -> [[ChunkingResult a]]
mkChunks' _ [] = []
mkChunks' chunkSize crs =
  if chunkSize >= length crs
    then [crs]
    else joinLastTwoChunksIfNeeded chunkSize (chunksOf chunkSize crs)

chunkChunks :: Int -> [Chunks a] -> Chunks a
chunkChunks chunkSize chunks =
  if length chunks <= chunkSize
    then Chunks chunks
    else
      let chunks' = joinLastTwoChunksIfNeeded chunkSize (chunksOf chunkSize chunks)
       in chunkChunks chunkSize (map Chunks chunks')

mkChunks :: ChunkSettings -> [a] -> Chunks a
mkChunks NoChunks as = toChunk as
mkChunks (SparseChunksOfSize chunkSize) imgs =
  let imgBuckets = mkSparseBuckets chunkSize imgs
   in chunkChunks chunkSize (map Chunk imgBuckets)
mkChunks (ChunkSize chunkSize) imgs =
  let imgChunks = mkChunks' chunkSize (map (`ChunkingResult` []) imgs)
   in chunkChunks chunkSize (map Chunk imgChunks)
mkChunks (ChunkTreeHeight h) as
  | h <= 1 = toChunk as
  | length as <= 1 = toChunk as
  | otherwise =
      let n = length as
          b = ceiling (fromIntegral n ** (1.0 / fromIntegral h) :: Double)
          chunkSize = max 2 b
          imgChunks = mkChunks' chunkSize (map (`ChunkingResult` []) as)
          unpackedImgChunks = map linearizeChunkingResults imgChunks
       in Chunks (map (\chunk -> mkChunks (ChunkTreeHeight (h - 1)) chunk) unpackedImgChunks)
