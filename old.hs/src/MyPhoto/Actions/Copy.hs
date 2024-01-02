module MyPhoto.Actions.Copy
    ( copyPAct
    , linkPAct
    , myphotooutPAct
    ) where

import           System.FilePath
import           System.Directory

import MyPhoto.Model
import MyPhoto.Utils

help :: PAction
help = PAction $ \_ -> pure (Left (unlines [ "copy TARGET_FOLDER"
                                           , "link TARGET_FOLDER"
                                           , "myphotoout"
                                           ]))

copyImpl :: FilePath -> [Img] -> PActionBody
copyImpl target imgs = do
  createDirectoryIfMissing True target

  imgs' <- mapM (\img -> let
                    img' = replaceDirectory img target
                  in do
                    copyFile img target
                    return img'
                ) imgs
  return (Right imgs')


copyPAct :: PrePAction
copyPAct ["-h"]   = help
copyPAct [target] = logSeparator ("Run copy (to " ++ target ++")") <> PAction (copyImpl target)
copyPAct _        = help

linkImpl :: FilePath -> [Img] -> PActionBody
linkImpl target imgs = do
  createDirectoryIfMissing True target

  imgs' <- mapM (\img -> let
                    img' = replaceDirectory img target
                  in do
                    createFileLink img img'
                    return img'
                ) imgs
  return (Right imgs')


linkPAct :: PrePAction
linkPAct ["-h"]   = help
linkPAct [target] = logSeparator ("Run link (to " ++ target ++")") <> PAction (linkImpl target)
linkPAct _        = help

findoutfile :: Int -> Img -> IO Img
findoutfile i outFile = do
  outFileExists <- doesFileExist outFile
  if outFileExists
    then do
      let (base,ext) = splitExtension outFile
      let outFilePlusI = base ++ "_" ++ show i ++ ext
      outFilePlusIExists <- doesFileExist outFilePlusI
      if outFilePlusIExists
        then findoutfile (i+1) outFile
        else return outFilePlusI
    else return outFile


myphotooutImpl' :: Img -> IO Img
myphotooutImpl' img = do
  let (dir,fn) = splitFileName img
      outDir = dir </> ".." </> "0_myphoto.out"
  createDirectoryIfMissing True outDir
  outFile <- findoutfile 0 (outDir </> fn)
  putStrLn (img ++ " -> " ++ outFile)
  copyFile img outFile
  return outFile
myphotooutImpl  :: [Img] -> PActionBody
myphotooutImpl imgs = do
  outs <- mapM myphotooutImpl' imgs
  return (Right outs)

myphotooutPAct :: PrePAction
myphotooutPAct ["-h"] = help
myphotooutPAct []     = logSeparator "Run myphotoout" <> PAction myphotooutImpl
myphotooutPAct _      = help
