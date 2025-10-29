module MyPhoto.Actions.FileSystem
  ( copy,
    move,
    link,
    reverseLink,
    removeRecursive,
  )
where

import MyPhoto.Model
import System.Directory (copyFile, createDirectoryIfMissing, createFileLink, removeDirectoryRecursive, renameFile)
import System.FilePath (replaceDirectory)

getPathInTargetFolder :: FilePath -> FilePath -> IO FilePath
getPathInTargetFolder target img = do
  createDirectoryIfMissing True target
  return (replaceDirectory img target)

applyFsFunc :: (Img -> Img -> IO Img) -> FilePath -> Imgs -> IO Imgs
applyFsFunc fsFunc target imgs = do
  imgs' <-
    mapM
      ( \img -> do
          img' <- getPathInTargetFolder target img
          fsFunc img img'
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
    ( \img img' -> do
        copyFile img img'
        return img'
    )
move =
  applyFsFunc
    ( \img img' -> do
        renameFile img img'
        return img'
    )
link =
  applyFsFunc
    ( \img img' -> do
        createFileLinkRelative img img'
        return img'
    )
reverseLink =
  applyFsFunc
    ( \img img' -> do
        renameFile img img'
        createFileLinkRelative img' img
        return img'
    )

removeRecursive :: FilePath -> IO ()
removeRecursive path = do
  removeDirectoryRecursive path
