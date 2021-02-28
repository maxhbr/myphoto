{-# LANGUAGE OverloadedStrings #-}
module MyPhoto.Actions.Thinning
    ( thinningPAct
    ) where

import           Control.Monad
import           System.Process
import           System.Exit
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Lazy as B


import MyPhoto.Model
import MyPhoto.Utils


help :: PAction
help = PAction $ \_ -> pure (Left ("thinning DELAY_IN_SECONDS"))

data Result
  = Result Img Int
  deriving Show
instance A.FromJSON Result where
  parseJSON =  A.withObject "Reslt" $ \v ->
    Result <$> v A..: "SourceFile" <*> v A..: "CreateDate"
{-
[{
  "SourceFile": "MAX09304.ARW",
  "CreateDate": 1614512491
},
{
  "SourceFile": "MAX09305.ARW",
  "CreateDate": 1614512494
}]
-}

mkArgs :: [Img] -> [String]
mkArgs imgs = ["-quiet", "-dateformat", "%s", "-json", "-CreateDate"] ++ imgs

thinningOnResults :: Int -> [Result] -> [Img]
thinningOnResults _        []     = []
thinningOnResults duration ((r@(Result img _)):rs) = let
    thinningOnResults' imgs' _ []                                            = imgs'
    thinningOnResults' imgs' (r1'@(Result _ time1)) ((r2'@(Result img' time2)):rs') = 
      if (abs (time1 - time2)) > duration
        then thinningOnResults' (imgs' ++ [img']) (r2') rs'
        else thinningOnResults' imgs' (r1') rs'
  in thinningOnResults' [img] r rs 

thinningImpl :: Int -> [Img] -> PActionBody
thinningImpl duration imgs = do
  (_, Just hout, _, pHandle) <- createProcess (proc "exiftool" (mkArgs imgs)) { std_out = CreatePipe }

  bs <- B.hGetContents hout

  exitCode <- waitForProcess pHandle
  unless (exitCode == ExitSuccess) $
    fail ("Crop failed with " ++ show exitCode)

  B.putStrLn bs

  case (A.eitherDecode bs :: Either String [Result]) of
    Right results -> return (Right (thinningOnResults duration results))
    Left err      -> fail ("Failed to decode: " ++ err)


thinningPAct :: PrePAction
thinningPAct ["-h"]     = help
thinningPAct [duration] = logSeparator ("Run thinning (with " ++ duration ++"s)") <> PAction (thinningImpl (read duration))
thinningPAct _          = help
