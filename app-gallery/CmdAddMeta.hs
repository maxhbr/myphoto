{-# LANGUAGE OverloadedStrings #-}

module CmdAddMeta (runAddMeta, parseAddArgs) where

import Control.Monad (unless)
import qualified Data.Set as Set
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Model (PhotoMeta (..), defaultDirMeta, defaultMeta, loadPhotoMeta, writePhotoMeta)
import Options.Applicative
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

data AddMetaOpts = AddMetaOpts
  { amoImg :: Maybe FilePath
  , amoTags :: [String]
  , amoAbout :: [FilePath]
  , amoPath :: Maybe FilePath
  , amoFiles :: [FilePath]
  }

parseAddArgs :: [String] -> Either String (PhotoMeta, [FilePath])
parseAddArgs argv =
  case execParserPure defaultPrefs parserInfo argv of
    Success opts -> Right (cliMeta opts, amoFiles opts)
    Failure failure -> Left (fst (renderFailure failure "myphoto-gallery"))
    CompletionInvoked _ -> Left "completion not supported"
  where
    parserInfo =
      info
        (addMetaParser <**> helper)
        (fullDesc <> progDesc "Create or extend sidecar metadata for images")

    addMetaParser =
      AddMetaOpts
        <$> optional (strOption (long "img" <> metavar "NAME" <> help "Override image basename"))
        <*> many (strOption (long "tag" <> metavar "TAG" <> help "Add tag (repeatable)"))
        <*> many (strOption (long "about" <> metavar "FILE" <> help "Add about image (repeatable)"))
        <*> optional (strOption (long "path" <> metavar "DIR" <> help "Set gallery subdirectory"))
        <*> some (argument str (metavar "FILE..."))

    cliMeta opts =
      mempty
        { img = amoImg opts
        , tags = Set.fromList (amoTags opts)
        , about = amoAbout opts
        , path = amoPath opts
        }

runAddMeta :: PhotoMeta -> FilePath -> IO ()
runAddMeta cliMeta filePath = do
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
          { img = img cliMeta <|> Just (takeFileName relPath)
          , tags = Set.union (tags baseMeta) (tags cliMeta)
          , about = about baseMeta ++ about cliMeta
          , path = path cliMeta <|> path baseMeta
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
