{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module CmdImport (runImport, runUpdate, runImportInit, runImportWithOpts, parseImportArgs, ImportOpts (..)) where

import Control.Concurrent.Async.Pool (withTaskGroup, mapTasks)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless, when)
import qualified Crypto.Hash as Hash
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.List (isPrefixOf, isSuffixOf)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import GHC.Conc (getNumCapabilities)
import Model
  ( ImportedMeta (..),
    GalleryConfig (..),
    PhotoMeta (..),
    defaultDirMeta,
    defaultGalleryConfig,
    loadImportedMeta,
    loadGalleryConfig,
    loadPhotoMeta,
    mergeMeta,
    resolveAboutPaths,
    writeImportedMeta,
    writeGalleryConfig,
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
    (-<.>),
  )
import System.IO (hPutStrLn, stderr)
import System.Process (callProcess)

metaSuffix :: String
metaSuffix = ".myphoto.toml"

importedSuffix :: String
importedSuffix = ".myphoto.imported.toml"

galleryBasePath :: FilePath
galleryBasePath = "./gallery"

galleryConfigPath :: FilePath
galleryConfigPath = "./myphoto.gallery.toml"

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
  cfg <- loadGalleryConfigOrDie
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
  runUpdateWithConfig ioDryRun cfg

runUpdate :: Bool -> IO ()
runUpdate ioDryRun = do
  cfg <- loadGalleryConfigOrDie
  runUpdateWithConfig ioDryRun cfg

runImportInit :: IO ()
runImportInit = do
  exists <- doesFileExist galleryConfigPath
  when exists (die ("Gallery config already exists: " <> galleryConfigPath))
  writeGalleryConfig galleryConfigPath defaultGalleryConfig
  putStrLn ("Wrote gallery config: " <> galleryConfigPath)


applyCfg :: GalleryConfig -> [(FilePath, PhotoMeta, a)] -> [(FilePath, PhotoMeta, a)]
applyCfg cfg = map apply
  where
    apply (imgPath, meta, a) =
      let remappedTags' = remappedTags cfg
          remappedPaths' = remappedPaths cfg
          ignoredTagsSet = ignoredTags cfg
          ignoredImgsSet = ignoredImgs cfg
          remapTag tag = Map.findWithDefault tag tag remappedTags'
          remapPath path = Map.findWithDefault path path remappedPaths'
          meta' = meta {
                    tags = Set.map remapTag (Set.filter (`Set.notMember` ignoredTagsSet) (tags meta)),
                    path = fmap remapPath (path meta),
                    ignore = if (Maybe.fromMaybe False (ignore meta)) || (Maybe.isJust (img meta) && Set.member (Maybe.fromMaybe "" (img meta)) ignoredImgsSet)
                              then Just True
                              else Nothing  
                  }
       in (imgPath, meta', a)

runUpdateWithConfig :: Bool -> GalleryConfig -> IO ()
runUpdateWithConfig ioDryRun cfg = do
  directoryExists <- doesDirectoryExist galleryBasePath
  unless directoryExists (die ("Gallery directory not found: " <> galleryBasePath))

  summaries <- applyCfg cfg <$> loadImportedSummaries galleryBasePath
  let summaries' = filter (\(_, meta, _) -> ignore meta /= Just True) summaries

  let uniqueTags = Set.toList $ Set.unions $ map (\(_, meta, _) -> tags meta) summaries'
  putStrLn $ "Found " ++ show (length summaries') ++ " imported images with " ++ show (length uniqueTags) ++ " unique tags."
  putStrLn $ "Unique tags: " ++ show uniqueTags

  when (not ioDryRun) $ do
    writeNanogalleries galleryBasePath summaries'
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
      if not sourceExists
        then hPutStrLn stderr ("Skipping (source missing): " <> absMetaPath)
        else case path merged of
          Nothing -> do
            die ("Error: no path specified in metadata: " <> absMetaPath)
          Just path' ->
            let destDir = galleryBasePath </> maybe (fallbackPath rootDir absMetaPath) id (path merged)
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
  capabilities <- getNumCapabilities
  let poolSize = min 6 $ max 1 (capabilities `div` 2)
  results <- let
      go summary = do
                          ret <- createScaledImage outDir width height summary
                          pb `incProgress` 1
                          pure ret
    in withTaskGroup poolSize $ \tg -> mapTasks tg (map go summaries)
  pure (catMaybes results)

createScaledImage :: FilePath -> Int -> Int -> (FilePath, PhotoMeta, String) -> IO  (Maybe (FilePath, PhotoMeta, String))
createScaledImage outDir width height (src, meta, srcHash) = let
      scaledRel = makeRelative galleryBasePath src -<.> ".png"
      scaled = outDir </> scaledRel
      hashPath = scaled <> ".md5"
      readHash = do
        ok <- doesFileExist hashPath
        if ok then fmap (Just . BSC.unpack) (BS.readFile hashPath) else pure Nothing
      matchesHash = do
        cached <- readHash
        destExists <- doesFileExist scaled
        pure (destExists && cached == Just srcHash)
      createScaled = do
              createDirectoryIfMissing True (takeDirectory scaled)
              let maxDimension = max width height
              let resizeArg = show maxDimension ++ "x" ++ show maxDimension ++ ">"
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
                        scaled
                      ]
                  ) ::
                  IO (Either SomeException ())
              case res of
                Left err -> do
                  hPutStrLn stderr ("[" ++ outDir ++ "] Failed for " <> src <> ": " <> show err)
                  pure False
                Right _ -> do
                  BS.writeFile hashPath (BSC.pack srcHash)
                  putStrLn $ "[" ++ outDir ++ "] Wrote " <> scaled
                  pure True
  in do
    let successReturnValue = Just (scaledRel, meta, srcHash)
        failureReturnValue = Nothing
    srcExists <- doesFileExist src
    if not srcExists
      then do
        hPutStrLn stderr ("[" ++ outDir ++ "] Source missing, skipping: " <> src)
        pure failureReturnValue
      else do
        scaledExists <- doesFileExist scaled

        success  <- if scaledExists
          then do
            destHashUpToDate <- matchesHash
            if destHashUpToDate
              then pure True
              else do
                hPutStrLn stderr ("[" ++ outDir ++ "] Scaled image outdated, recreating: " <> scaled)
                createScaled
          else do
            hPutStrLn stderr ("[" ++ outDir ++ "] Scaled image missing, will create: " <> scaled)
            createScaled
        pure $ if success
          then successReturnValue
          else failureReturnValue

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

loadGalleryConfigOrDie :: IO GalleryConfig
loadGalleryConfigOrDie = do
  exists <- doesFileExist galleryConfigPath
  unless exists (die ("Gallery config not found: " <> galleryConfigPath))
  parsed <- loadGalleryConfig galleryConfigPath
  case parsed of
    Left err -> die ("Failed to parse gallery config " <> galleryConfigPath <> ": " <> err)
    Right cfg -> pure cfg
