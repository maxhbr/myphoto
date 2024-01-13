module MyPhoto.Utils.Chunking
  where

import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.MSem as MS
import MyPhoto.Model
import Control.Monad
import Data.Char (toLower)
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe)
import System.Directory
import System.Exit
import System.FilePath
import System.Process


data Chunks
  = Chunk Imgs
  | Chunks [Chunks]
  deriving (Show, Eq)

showChunkTree :: Chunks -> String
showChunkTree (Chunk imgs) = show (length imgs)
showChunkTree (Chunks chunks) = show (length chunks) ++ " [" ++ (unwords (map showChunkTree chunks)) ++ "]"
linearizeChunks :: Chunks -> Imgs
linearizeChunks (Chunk imgs) = imgs
linearizeChunks (Chunks chunks) = concatMap linearizeChunks chunks

resolveChunks :: MS.MSem Int -> (FilePath -> Imgs -> IO (Either String Img)) -> FilePath -> Chunks -> IO (Either String Img)
resolveChunks sem f bn (Chunk imgs) = MS.with sem $ f bn imgs
resolveChunks sem f bn (Chunks chunks) = do
  let chunkSize = length chunks
  let chunkBn = \i -> bn ++ "_chunk" ++ show i ++ "of" ++ show chunkSize
  results <- mapConcurrently (\(i,c) -> resolveChunks sem f (chunkBn i) c) (zip [1 ..] chunks)
  let foldResults :: Either String [FilePath] -> Either String FilePath -> Either String [FilePath]
      foldResults (Left err1) (Left err2) = Left (unlines [err1, err2])
      foldResults r1@(Left _) _ = r1
      foldResults (Right imgs1) (Right img) = Right (imgs1 ++ [img])
      foldResults _ r2@(Left e) = Left e
  let result = foldl foldResults (Right []) results
  case result of
    Right imgs -> resolveChunks sem f bn (Chunk imgs)
    Left err -> return (Left err)

data ChunkSettings
  = ChunkSize Int
  | NoChunks
  deriving (Show, Eq)

instance Default ChunkSettings where
  def = ChunkSize 8

joinLastTwoChunksIfNeeded :: Int -> [[a]] -> [[a]]
joinLastTwoChunksIfNeeded _ [] = []
joinLastTwoChunksIfNeeded _ [chunk] = [chunk]
joinLastTwoChunksIfNeeded chunkSize chunks = let
    joinLastTwoChunks :: [[a]] -> [[a]]
    joinLastTwoChunks [] = []
    joinLastTwoChunks [chunk] = [chunk]
    joinLastTwoChunks [chunk1, chunk2] = [chunk1 ++ chunk2]
    joinLastTwoChunks (chunk1 : chunk2 : chunks) = chunk1 : joinLastTwoChunks (chunk2 : chunks)
  in if length (last chunks) > (chunkSize `div` 2)
     then chunks
     else joinLastTwoChunks chunks
mkChunks' :: Int -> [a] -> [[a]]
mkChunks' _ [] = []
mkChunks' chunkSize imgs = 
  if chunkSize >= length imgs
    then [imgs]
    else joinLastTwoChunksIfNeeded chunkSize (chunksOf chunkSize imgs)

chunkChunks :: Int -> [Chunks] -> Chunks
chunkChunks chunkSize chunks =
  if length chunks <= chunkSize
    then Chunks chunks
    else let
        chunks' = mkChunks' chunkSize chunks
      in chunkChunks chunkSize (map Chunks chunks')

mkChunks :: ChunkSettings -> Imgs -> Chunks
mkChunks NoChunks imgs = Chunk imgs
mkChunks (ChunkSize chunkSize) imgs = let
        imgChunks = mkChunks' chunkSize imgs
      in chunkChunks chunkSize (map Chunk imgChunks)
