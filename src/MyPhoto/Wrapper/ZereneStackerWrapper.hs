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

getCommonNameForOpts :: ZereneStackerOptions -> Maybe FilePath
getCommonNameForOpts opts =
  let commonBaseName = getCommonBaseName (_PMaxOutput opts) (_DMapOutput opts)
   in case commonBaseName of
        Just baseName -> case _Cwd opts of
          Just cwd -> Just (cwd </> baseName)
          Nothing -> Just baseName
        Nothing -> Nothing

getCommonBaseName :: Maybe Img -> Maybe Img -> Maybe FilePath
getCommonBaseName Nothing Nothing = Nothing
getCommonBaseName (Just pmax) Nothing = Just (takeBaseName pmax)
getCommonBaseName Nothing (Just dmap) = Just (takeBaseName dmap)
getCommonBaseName (Just pmax) (Just dmap) =
  let getCombined (a : as) (b : bs)
        | a == b = a : getCombined as bs
        | otherwise = (a : as) ++ (b : bs)
      getCombined as [] = as
      getCombined [] bs = bs
   in Just (getCombined (takeBaseName pmax) (takeBaseName dmap))

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
      commonName = getCommonNameForOpts opts
      projectArgs = case commonName of
        Just commonName ->
          [ "--project-folder",
            commonName -<.> "zsp-folder",
            "--prefix",
            commonName
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
      let logFile = case commonName of
            Just cn -> cn <.> "log"
            Nothing -> "/dev/null"
      logHandle <- openFile logFile WriteMode
      (_, _, _, ph) <- createProcess proc {std_out = UseHandle logHandle, std_err = UseHandle logHandle}
      ec <- waitForProcess ph
      hClose logHandle
      case ec of
        ExitSuccess -> return ()
        _ -> exitWith ec
