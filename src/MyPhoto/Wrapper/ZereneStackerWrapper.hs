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
    _PMaxOutput :: Maybe Img,
    _Cwd :: Maybe FilePath
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
  let proc = (System.Process.proc cmd (args ++ imgs)) {cwd = _Cwd opts}
  if _Verbose opts
    then do
      (_, _, _, ph) <- createProcess proc
      ec <- waitForProcess ph
      case ec of
        ExitSuccess -> return ()
        _ -> exitWith ec
    else do
      (_, _, _, ph) <- createProcess proc {std_out = CreatePipe, std_err = CreatePipe}
      _ <- waitForProcess ph
      return ()
