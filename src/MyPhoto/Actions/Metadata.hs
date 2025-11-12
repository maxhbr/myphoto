{-# LANGUAGE OverloadedStrings #-}

module MyPhoto.Actions.Metadata
  ( breaking,
    sortByCreateDate,
    getMetadataFromImg,
    getMetadataFromImgs,
    Metadata (..),
  )
where

import Control.Monad
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as Map
import Data.Text (Text, unpack)
import Data.Time
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.LocalTime
import Graphics.HsExif
import MyPhoto.Model hiding (Options (..))
import System.Exit
import System.ProgressBar (Progress (..), ProgressBar, defStyle, incProgress, newProgressBar)

data Metadata
  = Metadata
  { _img :: Img,
    _createDate :: Int,
    _flashFired :: Bool
  }
  deriving (Show)

instance A.FromJSON Metadata where
  parseJSON = A.withObject "Reslt" $ \v ->
    Metadata <$> v A..: "SourceFile" <*> v A..: "CreateDate" <*> v A..: "FlashFired"

getMetadataFromImg :: Bool -> Img -> IO Metadata
getMetadataFromImg verbose img = do
  when verbose $ putStrLn ("INFO: getting metadata of " ++ img)
  exifData <- parseFileExif img
  case exifData of
    Right exif -> do
      let createDate = case getDateTimeOriginal exif of
            Just localTime -> round (utcTimeToPOSIXSeconds (localTimeToUTC utc localTime))
            Nothing -> 0
          maybeValueToInt :: Maybe ExifValue -> Int
          maybeValueToInt (Just (ExifNumber i)) = i
          maybeValueToInt _ = 0
          flashFired = case wasFlashFired exif of
            Just True -> True
            _ -> False
      return (Metadata img createDate flashFired)
    Left errorMessage -> fail $ "Error reading EXIF data: " ++ errorMessage

getMetadataFromImgs :: Bool -> [Img] -> IO [Metadata]
getMetadataFromImgs verbose imgs = do
  pb <- newProgressBar defStyle 10 (Progress 0 (length imgs) ())
  mapM
    ( \img -> do
        metadata <- getMetadataFromImg verbose img
        incProgress pb 1
        return metadata
    )
    imgs

breakingOnMetadatas :: Int -> [Metadata] -> Imgs
breakingOnMetadatas _ [] = []
breakingOnMetadatas gapInSeconds ((r@(Metadata {_img = img})) : rs) =
  let breakingOnMetadatas' imgs' _ [] = imgs'
      breakingOnMetadatas' imgs' ((Metadata {_createDate = time1})) ((r2'@(Metadata {_img = img', _createDate = time2})) : rs') =
        if abs (time1 - time2) > gapInSeconds
          then imgs'
          else breakingOnMetadatas' (imgs' ++ [img']) r2' rs'
   in breakingOnMetadatas' [img] r rs

breaking :: Bool -> Int -> Imgs -> IO Imgs
breaking verbose gapInSeconds imgs = do
  metadatas <- getMetadataFromImgs verbose imgs
  return (breakingOnMetadatas gapInSeconds metadatas)

sortByCreateDate :: Bool -> Imgs -> IO Imgs
sortByCreateDate verbose imgs = do
  metadatas <- getMetadataFromImgs verbose imgs
  return (map (\(Metadata {_img = img}) -> img) (sortOn (\(Metadata {_createDate = time}) -> time) metadatas))
