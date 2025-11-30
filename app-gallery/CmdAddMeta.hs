{-# LANGUAGE OverloadedStrings #-}

module CmdAddMeta (runAddMeta) where

import Control.Monad (unless)
import qualified Data.Set as Set
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Model (PhotoMeta (..), defaultDirMeta, defaultMeta, loadPhotoMeta, writePhotoMeta)
import System.Directory
  ( doesFileExist
  , getModificationTime
  , makeAbsolute
  )
import System.Exit (die)
import System.FilePath
  ( makeRelative
  , takeDirectory
  , takeFileName
  , (</>)
  )
import System.IO (hPutStrLn, stderr)

metaSuffix :: String
metaSuffix = ".myphoto.toml"

runAddMeta :: Set.Set String -> [FilePath] -> FilePath -> IO ()
runAddMeta cliTags cliAbout filePath = do
  exists <- doesFileExist filePath
  unless exists (die ("File not found: " <> filePath))
  absPath <- makeAbsolute filePath
  let metaPath = filePath <> metaSuffix
      metaDir = takeDirectory metaPath
      relPath = makeRelative metaDir absPath
  modDate <- formatDate <$> getModificationTime filePath
  metaExists <- doesFileExist metaPath
  baseMeta <-
    if metaExists
      then do
        parsed <- loadPhotoMeta metaPath
        case parsed of
          Right m -> pure m
          Left err -> do
            hPutStrLn stderr ("Warning: could not parse existing metadata, regenerating: " <> err)
            pure (defaultMeta (takeFileName relPath) modDate)
      else pure (defaultMeta (takeFileName relPath) modDate)
  let newMeta =
        baseMeta
          { img = Just (takeFileName relPath)
          , tags = Set.union (tags baseMeta) cliTags
          , about = about baseMeta ++ cliAbout
          , modified = Just modDate
          }
  writePhotoMeta metaPath newMeta
  ensureDirectoryMeta metaDir modDate
  putStrLn $ "Metadata written to " <> metaPath

formatDate :: UTCTime -> String
formatDate = formatTime defaultTimeLocale "%F"

dirMetaPath :: FilePath -> FilePath
dirMetaPath dir = dir </> "myphoto.toml"

ensureDirectoryMeta :: FilePath -> String -> IO ()
ensureDirectoryMeta dir modDate = do
  let path' = dirMetaPath dir
  exists <- doesFileExist path'
  unless exists $
    writePhotoMeta path' (defaultDirMeta modDate)
