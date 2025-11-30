{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module CmdImport (runImport) where

import Control.Applicative ((<|>))
import Control.Monad (forM_, unless)
import qualified Data.Maybe as Maybe
import Data.List (isSuffixOf)
import Model
  ( PhotoMeta (..)
  , defaultDirMeta
  , loadPhotoMeta
  , loadImportedMeta
  , mergeMeta
  , resolveAboutPaths
  , writeImportedMeta
  , ImportedMeta (..)
  )
import Options.Applicative
import System.Directory
  ( copyFile
  , createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , listDirectory
  , makeAbsolute
  )
import System.Exit (die)
import System.FilePath
  ( isAbsolute
  , makeRelative
  , splitDirectories
  , splitExtension
  , takeDirectory
  , takeFileName
  , (</>)
  )
import System.IO (hPutStrLn, stderr)
import qualified Crypto.Hash as Hash
import qualified Data.ByteString as BS
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Maybe (catMaybes)

metaSuffix :: String
metaSuffix = ".myphoto.toml"
importedSuffix :: String
importedSuffix = ".myphoto.imported.toml"

data ImportOpts = ImportOpts
  { ioDryRun :: Bool
  , ioDir :: FilePath
  }

runImport :: FilePath -> IO ()
runImport dir = runImportWithOpts ImportOpts{ioDryRun = False, ioDir = dir}

runImportWithOpts :: ImportOpts -> IO ()
runImportWithOpts ImportOpts{ioDryRun, ioDir} = do
  let dir = ioDir
  ok <- doesDirectoryExist dir
  unless ok (die ("Directory not found: " <> dir))
  rootDir <- makeAbsolute dir
  metaFiles <- findMetaFiles dir
  if null metaFiles
    then putStrLn "No metadata files found."
    else forM_ metaFiles (importOne ioDryRun rootDir)
  _ <- loadImportedSummaries "."
  pure ()

findMetaFiles :: FilePath -> IO [FilePath]
findMetaFiles dir = do
  entries <- listDirectory dir
  fmap concat . mapM step $ entries
  where
    step entry = do
      let path' = dir </> entry
      isDir <- doesDirectoryExist path'
      if isDir
        then findMetaFiles path'
        else pure [path' | metaSuffix `isSuffixOf` path']

importOne :: Bool -> FilePath -> FilePath -> IO ()
importOne dryRun rootDir metaPath = do
  absMetaPath <- makeAbsolute metaPath
  let metaDir = takeDirectory absMetaPath
  metaResult <- loadPhotoMeta absMetaPath
  dirMeta <- loadDirMetaChain rootDir metaDir
  case metaResult of
    Left err ->
      hPutStrLn stderr ("Skipping malformed metadata " <> absMetaPath <> ": " <> err)
    Right meta -> do
      let merged = mergeMeta dirMeta (resolveAboutPaths metaDir meta)
          imgName = Maybe.fromMaybe (takeFileName (dropMetaSuffix absMetaPath)) (img merged)
          sourcePath = resolveSource metaDir imgName
      sourceExists <- doesFileExist sourcePath
      unless sourceExists $
        hPutStrLn stderr ("Skipping (source missing): " <> absMetaPath)
      if sourceExists
        then do
          let destDir = maybe (fallbackPath rootDir absMetaPath) id (path merged)
          unless dryRun (createDirectoryIfMissing True destDir)
          let target = destDir </> takeFileName sourcePath
          if dryRun
            then putStrLn $ "[dry-run] Would import: " <> sourcePath <> " -> " <> target
            else do
              copyFile sourcePath target
              newAbout <- copyAboutFiles destDir merged
              hashValue <- computeMd5 sourcePath
              now <- formatDate <$> getCurrentTime
              let importedMeta =
                    ImportedMeta
                      { original = merged
                      , overwrite = mempty
                      , imported = now
                      , md5 = hashValue
                      }
              writeImportedMeta (target <> importedSuffix) (importedMeta {original = (original importedMeta) {img = Just (takeFileName target), about = newAbout}})
              putStrLn $ "Imported: " <> sourcePath <> " -> " <> target
        else pure ()

resolveSource :: FilePath -> FilePath -> FilePath
resolveSource metaDir src =
  if isAbsolute src
    then src
    else metaDir </> src

dropMetaSuffix :: FilePath -> FilePath
dropMetaSuffix path' =
  reverse (drop (length metaSuffix) (reverse path'))

fallbackPath :: FilePath -> FilePath -> FilePath
fallbackPath root metaPath =
  case splitDirectories (makeRelative root (takeDirectory metaPath)) of
    (x : _) | not (null x) -> x
    _ -> "."

dirMetaPath :: FilePath -> FilePath
dirMetaPath dir = dir </> "myphoto.toml"

loadDirMeta :: FilePath -> IO PhotoMeta
loadDirMeta dir = do
  let path' = dirMetaPath dir
  exists <- doesFileExist path'
  if exists
    then do
      parsed <- loadPhotoMeta path'
      case parsed of
        Right m -> pure ((resolveAboutPaths dir m){img = Nothing})
        Left err -> do
          hPutStrLn stderr ("Warning: could not parse directory metadata, ignoring: " <> err)
          pure (defaultDirMeta "")
    else pure (defaultDirMeta "")

loadDirMetaChain :: FilePath -> FilePath -> IO PhotoMeta
loadDirMetaChain root dir = do
  dirs <- ascend [] =<< makeAbsolute dir
  metas <- mapM loadDirMeta dirs
  pure (foldl mergeMeta (defaultDirMeta "") metas)
  where
    ascend acc cur
      | cur == root = pure (reverse (cur : acc))
      | parent == cur = pure (reverse (cur : acc))
      | otherwise = ascend (cur : acc) parent
      where
        parent = takeDirectory cur

copyAboutFiles :: FilePath -> PhotoMeta -> IO [FilePath]
copyAboutFiles destDir meta = do
  let aboutDir = destDir </> "myphoto.about"
  createDirectoryIfMissing True aboutDir
  results <- mapM (copyOne aboutDir) (about meta)
  pure (Maybe.catMaybes results)
  where
    copyOne aboutDir src = do
      exists <- doesFileExist src
      if not exists
        then do
          hPutStrLn stderr ("Warning: about file missing, skipping: " <> src)
          pure Nothing
        else do
          let target = aboutDir </> takeFileName src
          copyFile src target
          let relPath = makeRelative destDir target
          pure (Just relPath)

loadImportedSummaries :: FilePath -> IO [(FilePath, PhotoMeta)]
loadImportedSummaries dir = do
  files <- findImportedFiles dir
  catMaybes <$> mapM parseOne files
  where
    parseOne fp = do
      parsed <- loadImportedMeta fp
      case parsed of
        Left err -> hPutStrLn stderr ("Warning: could not parse imported metadata " <> fp <> ": " <> err) >> pure Nothing
        Right im ->
          let merged = original im <> overwrite im
           in pure (Just (fp, merged))

findImportedFiles :: FilePath -> IO [FilePath]
findImportedFiles dir = do
  entries <- listDirectory dir
  fmap concat . mapM step $ entries
  where
    step entry = do
      let path' = dir </> entry
      isDir <- doesDirectoryExist path'
      if isDir
        then findImportedFiles path'
        else pure [path' | ".myphoto.imported.toml" `isSuffixOf` path']

computeMd5 :: FilePath -> IO String
computeMd5 fp = do
  bs <- BS.readFile fp
  let digest = Hash.hash bs :: Hash.Digest Hash.MD5
  pure (show digest)

formatDate :: UTCTime -> String
formatDate = formatTime defaultTimeLocale "%F"
