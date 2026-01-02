module MyPhoto.Actions.ZereneStacker
  ( zereneStackerImgs,
  )
where

import Data.Maybe (catMaybes)
import MyPhoto.Model
import MyPhoto.Wrapper.ZereneStackerWrapper
import qualified System.IO as IO

data ZereneStackerImagePlan
  = Planned FilePath
  | Done FilePath
  | NotPlanned
  deriving (Show, Eq)

fromFilePath :: FilePath -> IO ZereneStackerImagePlan
fromFilePath fp = do
  exists <- doesFileExist fp
  return $
    if exists
      then Done fp
      else Planned fp

toOpts :: ZereneStackerImagePlan -> Maybe FilePath
toOpts (Planned fp) = Just fp
toOpts (Done fp) = Nothing
toOpts NotPlanned = Nothing

toResult :: ZereneStackerImagePlan -> Maybe FilePath
toResult (Planned fp) = Just fp
toResult (Done fp) = Just fp
toResult NotPlanned = Nothing

isTodo :: ZereneStackerImagePlan -> Bool
isTodo (Planned _) = True
isTodo (Done _) = False
isTodo NotPlanned = False

zereneStackerImgs :: Bool -> Bool -> FilePath -> [FilePath] -> IO (Either String [FilePath])
zereneStackerImgs headless align outputBN imgs = do
  pmaxOutput' <- makeAbsolute (outputBN ++ "_zerene-PMax.tif")
  dmapOutput' <- makeAbsolute (outputBN ++ "_zerene-DMap.tif")

  pmaxOutput <- fromFilePath pmaxOutput'
  dmapOutput <-
    if headless
      then return NotPlanned
      else fromFilePath dmapOutput'

  if not (isTodo pmaxOutput) && not (isTodo dmapOutput)
    then do
      IO.hPutStrLn IO.stderr $ "INFO: Zerene Stacker outputs " ++ (show pmaxOutput) ++ " and " ++ (show dmapOutput) ++ " already exist, check that all aligned images are present"
    else do
      let opts =
            ZereneStackerOptions
              { _Headless = headless,
                _Align = align,
                _PMaxOutput = toOpts pmaxOutput,
                _DMapOutput = toOpts dmapOutput
              }
      runZereneStacker opts imgs

  return (Right (catMaybes [toResult pmaxOutput, toResult dmapOutput]))
