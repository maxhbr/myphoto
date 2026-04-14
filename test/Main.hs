{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

-- import Test.Hspec

import Control.Monad
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import Data.Default
import Data.FileEmbed (embedFile)
import qualified Data.Map as Map
import Data.Monoid
import MyPhoto.Actions.Align (parseCompareOffset)
import MyPhoto.Config ()
import MyPhoto.Model
import MyPhoto.Utils.Chunking
import Test.Framework as TF
import Test.Framework.Providers.HUnit
import Test.HUnit

imgs :: Imgs
imgs = map (\i -> show i ++ ".jpg") [0 .. 30]

imgChunks :: ChunkSettings -> Chunks String
imgChunks chunkSettings = mkChunks chunkSettings imgs

mkChunkingAssertion :: String -> ChunkSettings -> Chunks String -> Assertion
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
  mkChunkingAssertion "no chunking" NoChunks (toChunk imgs)
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

parseCompareOffsetTests :: [TF.Test]
parseCompareOffsetTests =
  [ testCase "typical NCC output" $
      assertEqual "should parse offset" (Just (10, 20)) (parseCompareOffset "0.998 (0.998) @ 10,20"),
    testCase "output with similarity tag" $
      assertEqual "should parse offset" (Just (5, 3)) (parseCompareOffset "0.95 (0.95) @ 5,3 [similar]"),
    testCase "zero offset" $
      assertEqual "should parse zero" (Just (0, 0)) (parseCompareOffset "1.0 (1.0) @ 0,0"),
    testCase "no @ symbol" $
      assertEqual "should return Nothing" Nothing (parseCompareOffset "0.998 (0.998) 10,20"),
    testCase "empty string" $
      assertEqual "should return Nothing" Nothing (parseCompareOffset ""),
    testCase "large offsets" $
      assertEqual "should parse large values" (Just (1234, 5678)) (parseCompareOffset "0.5 @ 1234,5678")
  ]

-- ############################################################################
-- ## Config parsing tests
-- ############################################################################

exampleOptionsJson :: BL.ByteString
exampleOptionsJson = BL.fromStrict $(embedFile "examples/options.json")

exampleOptionsPartialJson :: BL.ByteString
exampleOptionsPartialJson = BL.fromStrict $(embedFile "examples/options-partial.json")

exampleWatchOptionsJson :: BL.ByteString
exampleWatchOptionsJson = BL.fromStrict $(embedFile "examples/watch-options.json")

parseFullOptionsTest :: Assertion
parseFullOptionsTest = do
  case A.eitherDecode exampleOptionsJson of
    Left err -> assertFailure ("Failed to parse examples/options.json: " ++ err)
    Right opts -> assertEqual "full options should equal def" (def :: Options) opts

parsePartialOptionsTest :: Assertion
parsePartialOptionsTest = do
  case A.eitherDecode exampleOptionsPartialJson of
    Left err -> assertFailure ("Failed to parse examples/options-partial.json: " ++ err)
    Right opts -> do
      assertEqual "verbose" True (optVerbose opts)
      assertEqual "noGpu" True (optNoGpu opts)
      assertEqual "enfuseChunkSettings" NoChunks (optEnfuseChunkSettings opts)
      assertEqual "zereneStackerChunkSettings" (SparseChunksOfSize 4) (optZereneStackerChunkSettings opts)
      assertEqual "workdirStrategy" NextToImgFiles (optWorkdirStrategy opts)
      assertEqual "export" ExportToParent (optExport opts)
      assertEqual "clean" RemoveWorkdirRecursively (optClean opts)
      assertEqual "parameters" (Map.fromList [("focus-stack", ["--batchsize=6", "--threads=14"])]) (optParameters opts)
      -- Fields not in the partial config should keep defaults
      assertEqual "redirectLog stays default" (optRedirectLog (def :: Options)) (optRedirectLog opts)
      assertEqual "enfuse stays default" (optEnfuse (def :: Options)) (optEnfuse opts)
      assertEqual "downscalePct stays default" (optDownscalePct (def :: Options)) (optDownscalePct opts)

parseEmptyOptionsTest :: Assertion
parseEmptyOptionsTest = do
  case A.eitherDecode "{}" of
    Left err -> assertFailure ("Failed to parse empty object: " ++ err)
    Right opts -> assertEqual "empty object should equal def" (def :: Options) opts

roundtripOptionsTest :: Assertion
roundtripOptionsTest = do
  let encoded = A.encode (def :: Options)
  case A.eitherDecode encoded of
    Left err -> assertFailure ("Failed to roundtrip def: " ++ err)
    Right opts -> assertEqual "roundtrip should preserve def" (def :: Options) opts

parseWatchOptionsTest :: Assertion
parseWatchOptionsTest = do
  case A.eitherDecode exampleWatchOptionsJson of
    Left err -> assertFailure ("Failed to parse examples/watch-options.json: " ++ err)
    Right val ->
      -- Just verify it parses as a valid JSON Value with expected structure
      case val of
        A.Object _ -> return ()
        _ -> assertFailure "Expected a JSON object"

configTestCases :: [TF.Test]
configTestCases =
  [ testCase "parse full options.json" parseFullOptionsTest,
    testCase "parse partial options.json" parsePartialOptionsTest,
    testCase "parse empty object as Options" parseEmptyOptionsTest,
    testCase "roundtrip Options through JSON" roundtripOptionsTest,
    testCase "parse watch-options.json" parseWatchOptionsTest
  ]

main :: IO ()
main =
  defaultMainWithOpts
    [ testGroup "chunking" chunkTestCases,
      testGroup "parseCompareOffset" parseCompareOffsetTests,
      testGroup "config" configTestCases
    ]
    mempty
