module MyPhoto.Actions.FileSystem
  ( copy
  , move
  )
where

import System.Directory (copyFile, createDirectoryIfMissing, removeFile)
import System.FilePath (replaceDirectory)

import MyPhoto.Model

getPathInTargetFolder :: FilePath -> FilePath -> IO FilePath
getPathInTargetFolder target img = do
  createDirectoryIfMissing True target
  return (replaceDirectory img target)

copy :: FilePath -> Imgs -> IO Imgs
copy target imgs = do
  imgs' <-
    mapM
      ( \img -> do
                img' <- getPathInTargetFolder target img
                copyFile img target
                return img'
      )
      imgs
  return imgs'

move :: FilePath -> Imgs -> IO Imgs
move target imgs = do
  createDirectoryIfMissing True target
  imgs' <-
    mapM
      ( \img -> do
                img' <- getPathInTargetFolder target img
                copyFile img target
                removeFile img
                return img'
      )
      imgs
  return imgs'
