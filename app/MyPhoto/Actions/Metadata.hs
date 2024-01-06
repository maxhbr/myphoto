{-# LANGUAGE OverloadedStrings #-}

module MyPhoto.Actions.Metadata
  ( breaking
  , sortByCreateDate
  )
where

import Control.Monad
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Lazy as B
import MyPhoto.Model hiding (Options (..))
import System.Exit
import System.Process

import Data.Text (Text, unpack)
import Data.Time
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.LocalTime

import Graphics.HsExif

data Metadata
  = Metadata Img Int
  deriving (Show)

instance A.FromJSON Metadata where
  parseJSON = A.withObject "Reslt" $ \v ->
    Metadata <$> v A..: "SourceFile" <*> v A..: "CreateDate"

getMetadaFromImg :: Bool -> Img -> IO Metadata
getMetadaFromImg verbose img = do
  when verbose $ putStrLn ("INFO: getting metadata of " ++ img)
  exifData <- parseFileExif img
  case exifData of
    Right exif -> case getDateTimeOriginal exif of
      Just localTime -> do
        let seconds = round (utcTimeToPOSIXSeconds (localTimeToUTC utc localTime))
        return (Metadata img seconds)
      Nothing -> fail $ "Creation date not found in img: " ++ img
    Left errorMessage -> fail $ "Error reading EXIF data: " ++ errorMessage

getMetadaFromImgs :: Bool -> [Img] -> IO [Metadata]
getMetadaFromImgs verbose imgs = mapM (\img -> getMetadaFromImg verbose img) imgs

breakingOnMetadatas :: Int -> [Metadata] -> Imgs
breakingOnMetadatas _ [] = []
breakingOnMetadatas gapInSeconds ((r@(Metadata img _)) : rs) =
  let breakingOnMetadatas' imgs' _ [] = imgs'
      breakingOnMetadatas' imgs' (r1'@(Metadata _ time1)) ((r2'@(Metadata img' time2)) : rs') =
        if abs (time1 - time2) > gapInSeconds
          then imgs'
          else breakingOnMetadatas' (imgs' ++ [img']) r2' rs'
   in breakingOnMetadatas' [img] r rs

breaking :: Bool -> Int -> Imgs -> IO Imgs
breaking verbose gapInSeconds imgs = do
  metadatas <- getMetadaFromImgs verbose imgs
  return (breakingOnMetadatas gapInSeconds metadatas)


sortByCreateDate :: Bool -> Imgs -> IO Imgs
sortByCreateDate verbose imgs = do
  metadatas <- getMetadaFromImgs verbose imgs
  return (map (\(Metadata img _) -> img) (sortOn (\(Metadata _ time) -> time) metadatas))
