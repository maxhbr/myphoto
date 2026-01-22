module MyPhoto.Wrapper.ZereneStackerWrapper
  ( runZereneStacker,
    ZereneStackerOptions (..),
  )
where

import MyPhoto.Model
import System.Process

data ZereneStackerOptions = ZereneStackerOptions
  { _Headless :: Bool,
    _Verbose :: Bool,
    _Align :: Bool,
    _DMapOutput :: Maybe Img,
    _PMaxOutput :: Maybe Img
  }

runZereneStacker :: ZereneStackerOptions -> Imgs -> IO ()
runZereneStacker opts imgs = do
  let cmd = if _Headless opts then "zerene-stacker-batch-headless" else "zerene-stacker-batch"
      alignArgs = if not (_Align opts) then ["--already-aligned"] else []
      pmaxArgs =
        case _PMaxOutput opts of
          Just pmaxOutput -> ["--pmax-output", pmaxOutput]
          Nothing -> ["--no-pmax"]
      dmapArgs =
        case _DMapOutput opts of
          Just dmapOutput -> ["--dmap-output", dmapOutput]
          Nothing -> ["--no-dmap"]
      args = alignArgs ++ pmaxArgs ++ dmapArgs

  logDebugIO (unwords ["$", cmd, unwords args, "[img [img [...]]]"])
  if _Verbose opts
    then do
      callProcess cmd (args ++ imgs)
    else do
      _ <- readProcess cmd (args ++ imgs) ""
      return ()
