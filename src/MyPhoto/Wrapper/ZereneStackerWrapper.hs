module MyPhoto.Wrapper.ZereneStackerWrapper
  ( runZereneStacker,
    ZereneStackerOptions (..),
  )
where

import MyPhoto.Model
import System.FilePath (dropExtension)
import System.IO (IOMode (..), hClose, openFile)
import System.Process

data ZereneStackerOptions = ZereneStackerOptions
  { _Headless :: Bool,
    _Verbose :: Bool,
    _Wait :: Bool,
    _Align :: Bool,
    _DMapOutput :: Maybe Img,
    _PMaxOutput :: Maybe Img,
    _Cwd :: Maybe FilePath
  }

getProjectDir :: Maybe Img -> Maybe Img -> Maybe FilePath
getProjectDir Nothing Nothing = Nothing
getProjectDir (Just pmax) Nothing = Just (dropExtension pmax)
getProjectDir Nothing (Just dmap) = Just (dropExtension dmap)
getProjectDir (Just pmax) (Just dmap) =
  let getCombined (a : as) (b : bs)
        | a == b = a : getCombined as bs
        | otherwise = (a : as) ++ (b : bs)
      getCombined as [] = as
      getCombined [] bs = bs
   in Just (getCombined (dropExtension pmax) (dropExtension dmap))

runZereneStacker :: ZereneStackerOptions -> Imgs -> IO ()
runZereneStacker opts imgs = do
  let cmd = if _Headless opts then "zerene-stacker-batch-headless" else "zerene-stacker-batch"
      waitArg = if _Wait opts then ["--wait"] else []
      alignArgs = if not (_Align opts) then ["--already-aligned"] else []
      pmaxArgs =
        case _PMaxOutput opts of
          Just pmaxOutput -> ["--pmax-output", pmaxOutput]
          Nothing -> ["--no-pmax"]
      dmapArgs =
        case _DMapOutput opts of
          Just dmapOutput -> ["--dmap-output", dmapOutput]
          Nothing -> ["--no-dmap"]
      projectDir = getProjectDir (_PMaxOutput opts) (_DMapOutput opts)
      projectArgs = case projectDir of
        Just projectDir ->
          [ "--project-folder",
            projectDir -<.> "zsp-folder"
          ]
        Nothing -> []
      args = waitArg ++ alignArgs ++ pmaxArgs ++ dmapArgs ++ projectArgs

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
      devNull <- openFile "/dev/null" WriteMode
      (_, _, _, ph) <- createProcess proc {std_out = UseHandle devNull, std_err = UseHandle devNull}
      ec <- waitForProcess ph
      hClose devNull
      case ec of
        ExitSuccess -> return ()
        _ -> exitWith ec
