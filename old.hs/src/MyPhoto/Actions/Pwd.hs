module MyPhoto.Actions.Pwd
  ( pwdPAct,
  )
where

import MyPhoto.Model
import MyPhoto.Utils
import System.Directory

help :: PAction
help = PAction $ \_ -> pure (Left (unlines ["pwd TARGET_FOLDER"]))

pwdImpl :: FilePath -> [Img] -> PActionBody
pwdImpl target imgs = do
  setCurrentDirectory target
  return (Right imgs)

pwdPAct :: PrePAction
pwdPAct ["-h"] = help
pwdPAct [target] = logSeparator ("Change pwd (to " ++ target ++ ")") <> PAction (pwdImpl target)
pwdPAct _ = help
