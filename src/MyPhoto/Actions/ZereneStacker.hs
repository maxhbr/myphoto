module MyPhoto.Actions.ZereneStacker
  ( zereneStackerImgs,
  )
where

import MyPhoto.Model
import MyPhoto.Wrapper.ZereneStackerWrapper
import qualified System.IO as IO


zereneStackerImgs :: FilePath -> [FilePath] -> IO (Either String [FilePath])
zereneStackerImgs outputBN imgs = do
  pmaxOutput <- makeAbsolute (outputBN ++ "_zerene-PMax.tif")
  dmapOutput <- makeAbsolute (outputBN ++ "_zerene-DMap.tif")

  bothOutputsExist <- do
    pmaxExists <- doesFileExist pmaxOutput
    dmapExists <- doesFileExist dmapOutput
    return (pmaxExists && dmapExists)
  if bothOutputsExist
    then do
      IO.hPutStrLn IO.stderr $ "INFO: Zerene Stacker outputs " ++ pmaxOutput ++ " and " ++ dmapOutput ++ " already exist, check that all aligned images are present"
    else do
      runZereneStacker imgs pmaxOutput dmapOutput

  return (Right [pmaxOutput, dmapOutput])
