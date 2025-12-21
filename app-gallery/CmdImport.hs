{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module CmdImport (runImport, runUpdate, runImportWithOpts, parseImportArgs, ImportOpts (..)) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless, when)
import qualified Crypto.Hash as Hash
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.List (isPrefixOf, isSuffixOf)
import Data.Maybe (catMaybes)
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Model
  ( ImportedMeta (..),
    PhotoMeta (..),
    defaultDirMeta,
    loadImportedMeta,
    loadPhotoMeta,
    mergeMeta,
    resolveAboutPaths,
    writeImportedMeta,
  )
import MyPhoto.Utils.ProgressBar
  ( incProgress,
    newImgsProgressBar,
  )
import Nanogallery (writeNanogalleries)
import Options.Applicative
import System.Directory
  ( copyFile,
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    listDirectory,
    makeAbsolute,
  )
import System.Exit (die)
import System.FilePath
  ( isAbsolute,
    makeRelative,
    splitDirectories,
    takeDirectory,
    takeFileName,
    (</>),
  )
import System.IO (hPutStrLn, stderr)
import System.Process (callProcess)

metaSuffix :: String
metaSuffix = ".myphoto.toml"

importedSuffix :: String
importedSuffix = ".myphoto.imported.toml"

data ImportOpts = ImportOpts
  { ioDryRun :: Bool,
    ioDir :: FilePath
  }

parseImportArgs :: [String] -> Either String ImportOpts
parseImportArgs argv =
  case execParserPure defaultPrefs parserInfo argv of
    Success opts -> Right opts
    Failure failure -> Left (fst (renderFailure failure "myphoto-gallery import"))
    CompletionInvoked _ -> Left "completion not supported"
  where
    parserInfo =
      info
        (importParser <**> helper)
        (fullDesc <> progDesc "Import images using .myphoto.toml sidecars")
    importParser =
      ImportOpts
        <$> switch (long "dry-run" <> help "Print actions without copying/importing")
        <*> argument str (metavar "WORKDIR")

runImport :: FilePath -> IO ()
runImport dir = runImportWithOpts ImportOpts {ioDryRun = False, ioDir = dir}

runImportWithOpts :: ImportOpts -> IO ()
runImportWithOpts ImportOpts {ioDryRun, ioDir} = do
  let dir = ioDir
  ok <- doesDirectoryExist dir
  unless ok (die ("Directory not found: " <> dir))
  rootDir <- makeAbsolute dir
  metaFiles <- findMetaFiles dir
  if null metaFiles
    then putStrLn "No metadata files found."
    else do
      pb <- newImgsProgressBar metaFiles
      forM_
        metaFiles
        ( \metadataFile -> do
            importOne ioDryRun rootDir metadataFile
            pb `incProgress` 1
        )
  runUpdate ioDryRun

runUpdate :: Bool -> IO ()
runUpdate ioDryRun = do
  summaries <- loadImportedSummaries "."
  let summaries' = filter (\(_, meta, _) -> ignore meta /= Just True) summaries
  when (not ioDryRun) $ do
    writeNanogalleries "." summaries'
    (createScaledGallery "_4k" 3840 2160 summaries') >>= writeNanogalleries "./_4k"

-- (createScaledGallery "_1080p" 1920 1080 summaries') >>= writeNanogalleries "./_1080p"

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
      when sourceExists $
        let destDir = maybe (fallbackPath rootDir absMetaPath) id (path merged)
            target = destDir </> takeFileName sourcePath
         in if dryRun
              then putStrLn $ "[dry-run] Would import: " <> sourcePath <> " -> " <> target
              else do
                createDirectoryIfMissing True destDir
                copyFile sourcePath target
                newAbout <- copyAboutFiles destDir merged
                hashValue <- computeMd5 sourcePath
                now <- formatDate <$> getCurrentTime
                let importedMetaFilePath = target <> importedSuffix
                mayebeExistingOverwrite <- do
                  exists <- doesFileExist importedMetaFilePath
                  if exists
                    then do
                      parsed <- loadImportedMeta importedMetaFilePath
                      case parsed of
                        Left err -> hPutStrLn stderr ("Warning: could not parse existing imported metadata " <> importedMetaFilePath <> ": " <> err) >> pure Nothing
                        Right im -> pure (Just (overwrite im))
                    else pure Nothing
                let importedMeta =
                      ImportedMeta
                        { original = merged,
                          overwrite = maybe (mempty {tags = Set.fromList ["New"]}) id mayebeExistingOverwrite,
                          imported = now,
                          md5 = hashValue,
                          originalPath = Just (makeRelative rootDir (takeDirectory sourcePath))
                        }
                writeImportedMeta importedMetaFilePath (importedMeta {original = (original importedMeta) {img = Just (takeFileName target), about = newAbout}})
                putStrLn $ "Imported: " <> sourcePath <> " -> " <> target

resolveSource :: FilePath -> FilePath -> FilePath
resolveSource metaDir src =
  if isAbsolute src
    then src
    else metaDir </> src

dropMetaSuffix :: FilePath -> FilePath
dropMetaSuffix path' = reverse (drop (length metaSuffix) (reverse path'))

dropImportedMetaSuffix :: FilePath -> FilePath
dropImportedMetaSuffix path' = reverse (drop (length importedSuffix) (reverse path'))

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
        Right m -> pure ((resolveAboutPaths dir m) {img = Nothing})
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
  let aboutDir = destDir </> "_myphoto.about"
  results <- mapM (copyOne aboutDir) (about meta)
  let kept = Maybe.catMaybes results
  pure kept
  where
    copyOne aboutDir src = do
      exists <- doesFileExist src
      if not exists
        then do
          hPutStrLn stderr ("Warning: about file missing, skipping: " <> src)
          pure Nothing
        else do
          createDirectoryIfMissing True aboutDir
          let target = aboutDir </> takeFileName src
          copyFile src target
          let relPath = makeRelative destDir target
          pure (Just relPath)

loadImportedSummaries :: FilePath -> IO [(FilePath, PhotoMeta, String)]
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
              imgPath = dropImportedMetaSuffix fp
           in pure (Just (imgPath, merged, md5 im))

createScaledGallery :: FilePath -> Int -> Int -> [(FilePath, PhotoMeta, String)] -> IO [(FilePath, PhotoMeta, String)]
createScaledGallery outDir width height summaries = do
  pb <- newImgsProgressBar summaries
  results <- mapConcurrently (go pb) summaries
  pure (catMaybes results)
  where
    go pb (src, meta, srcHash) = do
      let rel = makeRelative "." src
          dest = outDir </> rel
          hashPath = dest <> ".md5"
      exists <- doesFileExist src
      ret <-
        if not exists
          then do
            hPutStrLn stderr ("[" ++ outDir ++ "] Source missing, skipping: " <> src)
            pure Nothing
          else do
            cached <- readHash hashPath
            destExists <- doesFileExist dest
            if destExists && cached == Just srcHash
              then pure (Just (src, meta, srcHash))
              else do
                createDirectoryIfMissing True (takeDirectory dest)
                let resizeArg = show width ++ "x" ++ show height ++ ">"
                res <-
                  try
                    ( callProcess
                        "magick"
                        [ src,
                          "-resize",
                          resizeArg,
                          "-unsharp",
                          "1.5x1.2+1.0+0.10",
                          "-interlace",
                          "Plane",
                          "-strip",
                          "-quality",
                          "95",
                          dest
                        ]
                    ) ::
                    IO (Either SomeException ())
                case res of
                  Left err -> hPutStrLn stderr ("[" ++ outDir ++ "] Failed for " <> src <> ": " <> show err) >> pure Nothing
                  Right _ -> do
                    BS.writeFile hashPath (BSC.pack srcHash)
                    putStrLn $ "[" ++ outDir ++ "] Wrote " <> dest
                    pure (Just (src, meta, srcHash))
      pb `incProgress` 1
      pure ret
    readHash p = do
      ok <- doesFileExist p
      if ok then fmap (Just . BSC.unpack) (BS.readFile p) else pure Nothing

findImportedFiles :: FilePath -> IO [FilePath]
findImportedFiles dir = do
  entries <- listDirectory dir
  fmap concat . mapM step $ entries
  where
    step entry = do
      let path' = dir </> entry
      isDir <- doesDirectoryExist path'
      let dirBasename = takeFileName path'
      if isDir && not (isPrefixOf "_" dirBasename)
        then findImportedFiles path'
        else pure [path' | importedSuffix `isSuffixOf` path']

computeMd5 :: FilePath -> IO String
computeMd5 fp = do
  bs <- BS.readFile fp
  let digest = Hash.hash bs :: Hash.Digest Hash.MD5
  pure (show digest)

formatDate :: UTCTime -> String
formatDate = formatTime defaultTimeLocale "%F"
