module MyPhoto.Wrapper.ZereneStackerWrapper
  ( runZereneStacker,
    ZereneStackerOptions (..),
  )
where

import MyPhoto.Model
import System.Process

data ZereneStackerOptions = ZereneStackerOptions
  { _Headless :: Bool,
    _Align :: Bool,
    _DMapOutput :: Maybe Img,
    _PMaxOutput :: Maybe Img
  }

runZereneStacker :: ZereneStackerOptions -> Imgs -> IO ()
runZereneStacker opts imgs = do
  let headlessArgs = if _Headless opts then ["--headless"] else []
      alignArgs = if not (_Align opts) then ["--already-aligned"] else []
      pmaxArgs =
        case _PMaxOutput opts of
          Just pmaxOutput -> ["--pmax-output", pmaxOutput]
          Nothing -> ["--no-pmax"]
      dmapArgs =
        case _DMapOutput opts of
          Just dmapOutput -> ["--dmap-output", dmapOutput]
          Nothing -> ["--no-dmap"]
      args = headlessArgs ++ alignArgs ++ pmaxArgs ++ dmapArgs

  logDebugIO (unwords ["$ zerene-stacker-batch", unwords args, "[img [img [...]]]"])
  _ <- readProcess "zerene-stacker-batch" (args ++ imgs) ""
  return ()
