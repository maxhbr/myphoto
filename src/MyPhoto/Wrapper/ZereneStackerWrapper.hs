module MyPhoto.Wrapper.ZereneStackerWrapper
  ( runZereneStacker
  )
where

import MyPhoto.Model
import System.Process

runZereneStacker :: Imgs -> Img -> Img -> IO ()
runZereneStacker imgs pmaxOutput dmapOutput = do
    let args = ["--pmax-aligned", pmaxOutput, "--dmap-aligned", dmapOutput]
    logDebugIO (unwords ["$ zerene-stacker-batch", unwords args, "[img [img [...]]]"])
    _ <- readProcess "zerene-stacker-batch" (args ++ imgs) ""
    return ()