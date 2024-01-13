{-# LANGUAGE LambdaCase #-}
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
import MyPhoto.Wrapper.EnblendEnfuseWrapper
import MyPhoto.Utils.Chunking
import MyPhoto.Actions.FileSystem (move)
import System.Console.GetOpt
import System.Directory
import System.Exit
import System.FilePath
import System.Process


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
  def =
    EnblendEnfuseActionOptions
      { eeOptions = def,
        eeaMaxCapabilities = 9, -- with to many threads the memory seems to be insufficient
        eeaOutputBN = Nothing,
        eeaChunk = def,
        eeaConcurrent = True,
        eeaAll = False
      }

optionsToFilenameAppendix :: EnblendEnfuseActionOptions -> String
optionsToFilenameAppendix EnblendEnfuseActionOptions {eeOptions = o} =
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
getStackedFilename opts (img : _) =
  let (bn, ext') = splitExtensions img
      ext = if map toLower ext' == ".jpg" then ".png" else ext
   in case eeaOutputBN opts of
        Nothing -> bn ++ "_enfuse" ++ optionsToFilenameAppendix opts <.> ext
        Just outputBN -> outputBN ++ "_enfuse" ++ optionsToFilenameAppendix opts <.> ext

enfuseStackImgs :: EnblendEnfuseActionOptions -> [FilePath] -> IO (Either String [FilePath])
enfuseStackImgs =
  let stackImpl' :: MS.MSem Int -> EnblendEnfuseActionOptions -> [FilePath] -> IO (Either String [Img])
      stackImpl' sem opts imgs = do
        let outFile' = getStackedFilename opts imgs
        outFile <- makeAbsolute outFile'

        outFileExists <- doesFileExist outFile
        if outFileExists
          then do
            putStrLn ("#### " ++ outFile ++ " already exists, skipping")
            return (Right [outFile])
          else do
            putStrLn ("#### calculate " ++ outFile ++ "...")

            let workdir = outFile <.> "workdir"
            createDirectoryIfMissing True workdir

            putStrLn ("##### compute Chunks ...")
            let chunks = mkChunks (eeaChunk opts) imgs
            putStrLn ("##### chunks: " ++ showChunkTree chunks)

            putStrLn ("##### resolve Chunks ...")
            let (bn, ext) = splitExtensions (takeBaseName outFile)
            let bnInWorkdir = workdir </> bn
            result <- resolveChunks sem (\bn imgs -> do
              let outFile = bn <.> ext
              runEnblendEnfuseWithRetries 2 (eeOptions opts) outFile workdir imgs
              ) bnInWorkdir chunks

            case result of
              Right generatedInWorkdir -> do
                renameFile generatedInWorkdir outFile
                return (Right [outFile])
              Left err -> do
                return (Left err)

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
                [ opts {eeaAll = False, eeOptions = eeOpts {eeProjection = Proj1, eeOpts = Opts1}},
                  opts {eeaAll = False, eeOptions = eeOpts {eeProjection = Proj1, eeOpts = Opts2}},
                  opts {eeaAll = False, eeOptions = eeOpts {eeProjection = Proj1, eeOpts = Opts3}},
                  opts {eeaAll = False, eeOptions = eeOpts {eeProjection = Proj2, eeOpts = Opts1}},
                  opts {eeaAll = False, eeOptions = eeOpts {eeProjection = Proj2, eeOpts = Opts2}},
                  opts {eeaAll = False, eeOptions = eeOpts {eeProjection = Proj2, eeOpts = Opts3}},
                  opts {eeaAll = False, eeOptions = eeOpts {eeProjection = Proj3, eeOpts = Opts1}},
                  opts {eeaAll = False, eeOptions = eeOpts {eeProjection = Proj3, eeOpts = Opts2}},
                  opts {eeaAll = False, eeOptions = eeOpts {eeProjection = Proj3, eeOpts = Opts3}}
                ]
            return (foldl foldResults (Right []) results)
          else stackImpl' sem opts imgs
