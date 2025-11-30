{-# LANGUAGE OverloadedStrings #-}

module Model
  ( PhotoMeta (..)
  , ImportedMeta (..)
  , defaultMeta
  , defaultDirMeta
  , mergeMeta
  , writePhotoMeta
  , writeImportedMeta
  , loadPhotoMeta
  , resolveAboutPaths
  ) where

import Control.Applicative ((<|>))
import Data.Bifunctor (first)
import qualified Data.Set as Set
import qualified Data.Text.IO as TIO
import System.FilePath (isAbsolute, makeRelative, takeDirectory, (</>))
import qualified Toml

data ImportedMeta = ImportedMeta
  { original :: PhotoMeta
  , overwrite :: PhotoMeta
  , imported :: String
  , md5 :: String
  }
  deriving (Show, Eq)

-- Core metadata used by both file and directory TOML files.
data PhotoMeta = PhotoMeta
  { img :: Maybe FilePath
  , tags :: Set.Set String
  , path :: Maybe FilePath
  , about :: [FilePath] -- always absolute paths, which are never serialized
  , modified :: Maybe String
  }
  deriving (Show, Eq)

instance Semigroup PhotoMeta where
  a <> b =
    PhotoMeta
      { img = img b <|> img a
      , tags = tags a <> tags b
      , path = path b <|> path a
      , about = about b <> about a
      , modified = modified b <|> modified a
      }

instance Monoid PhotoMeta where
  mempty =
    PhotoMeta
      { img = Nothing
      , tags = Set.empty
      , path = Nothing
      , about = []
      , modified = Nothing
      }

data PhotoMetaPayload = PhotoMetaPayload
  { payloadImg :: Maybe FilePath
  , payloadTags :: [String]
  , payloadPath :: Maybe FilePath
  , payloadAbout :: [FilePath] -- always relative paths, relative to the TOML file
  , payloadModified :: Maybe String
  }
  deriving (Show, Eq)

data ImportedMetaPayload = ImportedMetaPayload
  { payloadOriginal :: PhotoMetaPayload
  , payloadOverwrite :: PhotoMetaPayload
  , payloadImported :: String
  , payloadMd5 :: String
  }
  deriving (Show, Eq)

defaultMeta :: FilePath -> String -> PhotoMeta
defaultMeta relImg modDate =
  PhotoMeta
    { img = Just relImg
    , tags = Set.empty
    , path = Nothing
    , about = []
    , modified = Just modDate
    }

defaultDirMeta :: String -> PhotoMeta
defaultDirMeta modDate =
  PhotoMeta
    { img = Nothing
    , tags = Set.empty
    , path = Nothing
    , about = []
    , modified = Just modDate
    }

mergeMeta :: PhotoMeta -> PhotoMeta -> PhotoMeta
mergeMeta = (<>)

writePhotoMeta :: FilePath -> PhotoMeta -> IO ()
writePhotoMeta path' meta =
  TIO.writeFile path' (Toml.encode photoMetaPayloadCodec (toPayload path' meta))

writeImportedMeta :: FilePath -> ImportedMeta -> IO ()
writeImportedMeta path' meta =
  TIO.writeFile path' (Toml.encode importedMetaPayloadCodec (toImportedPayload path' meta))

loadPhotoMeta :: FilePath -> IO (Either String PhotoMeta)
loadPhotoMeta path' = do
  content <- TIO.readFile path'
  pure (first renderErrors (fmap fromPayload (Toml.decode photoMetaPayloadCodec content)))

photoMetaPayloadCodec :: Toml.TomlCodec PhotoMetaPayload
photoMetaPayloadCodec =
  PhotoMetaPayload
    <$> Toml.dioptional (Toml.string "img") Toml..= payloadImg
    <*> Toml.arrayOf Toml._String "tags" Toml..= payloadTags
    <*> Toml.dioptional (Toml.string "path") Toml..= payloadPath
    <*> Toml.arrayOf Toml._String "about" Toml..= payloadAbout
    <*> Toml.dioptional (Toml.string "modified") Toml..= payloadModified

toPayload :: FilePath -> PhotoMeta -> PhotoMetaPayload
toPayload tomlPath meta =
  PhotoMetaPayload
    { payloadImg = img meta
    , payloadTags = Set.toList (tags meta)
    , payloadPath = path meta
    , payloadAbout = map (makeRelative (takeDirectory tomlPath)) (about meta)
    , payloadModified = modified meta
    }

fromPayload :: PhotoMetaPayload -> PhotoMeta
fromPayload payload =
  PhotoMeta
    { img = payloadImg payload
    , tags = Set.fromList (payloadTags payload)
    , path = payloadPath payload
    , about = payloadAbout payload
    , modified = payloadModified payload
    }

importedMetaPayloadCodec :: Toml.TomlCodec ImportedMetaPayload
importedMetaPayloadCodec =
  ImportedMetaPayload
    <$> Toml.table photoMetaPayloadCodec "original" Toml..= payloadOriginal
    <*> Toml.table photoMetaPayloadCodec "overwrite" Toml..= payloadOverwrite
    <*> Toml.string "imported" Toml..= payloadImported
    <*> Toml.string "md5" Toml..= payloadMd5

toImportedPayload :: FilePath -> ImportedMeta -> ImportedMetaPayload
toImportedPayload tomlPath meta =
  let baseDir = takeDirectory tomlPath
   in ImportedMetaPayload
        { payloadOriginal = toPayload baseDir (original meta)
        , payloadOverwrite = toPayload baseDir (overwrite meta)
        , payloadImported = imported meta
        , payloadMd5 = md5 meta
        }

renderErrors :: [Toml.TomlDecodeError] -> String
renderErrors =
  unlines . map show

resolveAboutPaths :: FilePath -> PhotoMeta -> PhotoMeta
resolveAboutPaths baseDir meta =
  meta {about = map (rel baseDir) (about meta)}
  where
    rel dir p =
      if isAbsolute p then p else dir </> p
