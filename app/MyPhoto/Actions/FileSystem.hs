module MyPhoto.Actions.FileSystem
  ( copy
  , move
  , link
  , reverseLink
  )
where

import System.Directory (copyFile, createDirectoryIfMissing, removeFile, createFileLink)
import System.FilePath (replaceDirectory)

import MyPhoto.Model

getPathInTargetFolder :: FilePath -> FilePath -> IO FilePath
getPathInTargetFolder target img = do
  createDirectoryIfMissing True target
  return (replaceDirectory img target)

applyFsFunc :: (FilePath -> Img -> Img -> IO Img) -> FilePath -> Imgs -> IO Imgs
applyFsFunc fsFunc target imgs = do
  imgs' <- mapM (\ img -> do
                          img' <- getPathInTargetFolder target img
                          fsFunc target img img') imgs
  return imgs'

copy, move, link, reverseLink :: FilePath -> Imgs -> IO Imgs
copy = applyFsFunc (\target img img' -> copyFile img img' >> return img' )
move = applyFsFunc (\target img img' -> copyFile img img' >> removeFile img >> return img' )
link = applyFsFunc (\target img img' -> createFileLink img img' >> return img' )
reverseLink = applyFsFunc (\target img img' -> copyFile img img' >> removeFile img >> createFileLink img' img >> return img' )
