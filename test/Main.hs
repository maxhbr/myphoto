module Main (main) where

-- import Test.Hspec

import Control.Monad
import Data.Monoid
import MyPhoto.Model
import MyPhoto.Utils.Chunking
import Test.Framework as TF
import Test.Framework.Providers.HUnit
import Test.HUnit

imgs :: Imgs
imgs = map (\i -> show i ++ ".jpg") [0 .. 30]

imgChunks :: ChunkSettings -> Chunks
imgChunks chunkSettings = mkChunks chunkSettings imgs

mkChunkingAssertion :: String -> ChunkSettings -> Chunks -> Assertion
mkChunkingAssertion description chunkSettings expected = do
  let actual = imgChunks chunkSettings
  assertEqual description expected actual

mkChunkingShowTreeAssertion :: String -> ChunkSettings -> String -> Assertion
mkChunkingShowTreeAssertion description chunkSettings expected = do
  let actual = showChunkTree (imgChunks chunkSettings)
  assertEqual description expected actual

mkChunkingConsistencyAssertion :: String -> ChunkSettings -> Assertion
mkChunkingConsistencyAssertion description chunkSettings = do
  let chunks = imgChunks chunkSettings
  let imgs' = linearizeChunks chunks
  assertEqual description imgs imgs'

noChunkingTest :: Assertion
noChunkingTest = do
  mkChunkingAssertion "no chunking" NoChunks (Chunk imgs)
  mkChunkingShowTreeAssertion "no chunking" NoChunks (show (length imgs))
  mkChunkingConsistencyAssertion "no chunking" NoChunks

fixedChunkingTest :: Assertion
fixedChunkingTest = do
  mkChunkingShowTreeAssertion "fixed chunking" (ChunkSize 3) "3 [3 [3 3 3] 3 [3 3 3] 4 [3 3 3 4]]"
  mkChunkingConsistencyAssertion "fixed chunking" NoChunks

chunkTestCases :: [TF.Test]
chunkTestCases =
  [ testCase "no chunking" noChunkingTest,
    testCase "fixed chunking" fixedChunkingTest
  ]

main :: IO ()
main =
  defaultMainWithOpts
    [testGroup "chunking" chunkTestCases]
    -- [testCase "push" pushTest
    -- ,testCase "push-pop" pushPopTest]
    mempty
