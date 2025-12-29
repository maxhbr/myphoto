module MyPhoto.Wrapper.ZereneStackerWrapper
  ( runZereneStacker,
    ZereneStackerOptions (..)
  )
where

import MyPhoto.Model
import System.Process

data ZereneStackerOptions = ZereneStackerOptions
  { _Headless :: Bool
  , _Align :: Bool
  , _DMapOutput :: Maybe Img
  , _PMaxOutput :: Maybe Img
  }

runZereneStacker :: ZereneStackerOptions -> Imgs -> IO ()
runZereneStacker opts imgs = do
  let pmaxArgs =
        case _PMaxOutput opts of
          Just pmaxOutput -> [(if _Align opts then "--pmax-aligned" else "--pmax-unaligned"), pmaxOutput]
          Nothing -> []
      dmapArgs =
        case _DMapOutput opts of
          Just dmapOutput -> [(if _Align opts then "--dmap-aligned" else "--dmap-unaligned"), dmapOutput]
          Nothing -> []
  case _Headless opts of
    True -> do
      case pmaxArgs of
        [] -> error "PMax output must be specified in headless mode"
        _ -> return ()
      case dmapArgs of
        [] -> return ()
        _ -> error "DMap output cannot be specified in headless mode"
      logDebugIO (unwords ["$ zerene-stacker-batch", unwords pmaxArgs, "[img [img [...]]]"])
      _ <- readProcess "xvfb-run" (["-a", "zerene-stacker-batch"] ++ pmaxArgs ++ imgs) ""
      return ()
    False -> do
      let args = pmaxArgs ++ dmapArgs
      case args of
        [] -> error "At least one of PMax or DMap output must be specified in GUI mode"
        _ -> do
                logDebugIO (unwords ["$ zerene-stacker-batch", unwords args, "[img [img [...]]]"])
                _ <- readProcess "zerene-stacker-batch" (args ++ imgs) ""
                return ()