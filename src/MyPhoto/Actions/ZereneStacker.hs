module MyPhoto.Actions.ZereneStacker
  ( zereneStackerImgs,
  )
where

import Data.Maybe (catMaybes)
import MyPhoto.Model
import MyPhoto.Wrapper.ZereneStackerWrapper
import qualified System.IO as IO

zereneStackerImgs :: Bool -> Bool -> FilePath -> [FilePath] -> IO (Either String [FilePath])
zereneStackerImgs headless align outputBN imgs = do
  pmaxOutput' <- makeAbsolute (outputBN ++ "_zerene-PMax.tif")
  dmapOutput' <- makeAbsolute (outputBN ++ "_zerene-DMap.tif")

  let pmaxOutput = Just pmaxOutput'
      dmapOutput = if headless then Nothing else Just dmapOutput'

  bothOutputsExistOrNotRequired <- do
    pmaxExistsOrNotRequired <- case pmaxOutput of
      Just pmaxPath -> doesFileExist pmaxPath
      Nothing -> return True
    dmapExistsOrNotRequired <- case dmapOutput of
      Just dmapPath -> doesFileExist dmapPath
      Nothing -> return True
    return (pmaxExistsOrNotRequired && dmapExistsOrNotRequired)
  if bothOutputsExistOrNotRequired
    then do
      IO.hPutStrLn IO.stderr $ "INFO: Zerene Stacker outputs " ++ (show pmaxOutput) ++ " and " ++ (show dmapOutput) ++ " already exist, check that all aligned images are present"
    else do
      let opts =
            ZereneStackerOptions
              { _Headless = headless,
                _Align = align,
                _PMaxOutput = pmaxOutput,
                _DMapOutput = dmapOutput
              }
      runZereneStacker opts imgs

  return (Right (catMaybes [pmaxOutput, dmapOutput]))
