module MyPhoto.Actions.FileSystem
  ( copy,
    move,
    link,
    reverseLink,
  )
where

import MyPhoto.Model
import System.Directory (copyFile, createDirectoryIfMissing, createFileLink, renameFile)
import System.FilePath (replaceDirectory)

getPathInTargetFolder :: FilePath -> FilePath -> IO FilePath
getPathInTargetFolder target img = do
  createDirectoryIfMissing True target
  return (replaceDirectory img target)

applyFsFunc :: (FilePath -> Img -> Img -> IO Img) -> FilePath -> Imgs -> IO Imgs
applyFsFunc fsFunc target imgs = do
  imgs' <-
    mapM
      ( \img -> do
          img' <- getPathInTargetFolder target img
          fsFunc target img img'
      )
      imgs
  return imgs'

createFileLinkRelative :: FilePath -> FilePath -> IO ()
createFileLinkRelative img img' = do
  let img'' = makeRelative (takeDirectory img') img
  createFileLink img'' img'

copy, move, link, reverseLink :: FilePath -> Imgs -> IO Imgs
copy =
  applyFsFunc
    ( \target img img' -> do
        copyFile img img'
        return img'
    )
move =
  applyFsFunc
    ( \target img img' -> do
        renameFile img img'
        return img'
    )
link =
  applyFsFunc
    ( \target img img' -> do
        createFileLinkRelative img img'
        return img'
    )
reverseLink =
  applyFsFunc
    ( \target img img' -> do
        renameFile img img'
        createFileLinkRelative img' img
        return img'
    )
