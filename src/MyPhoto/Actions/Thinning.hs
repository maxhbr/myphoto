{-# LANGUAGE OverloadedStrings #-}
module MyPhoto.Actions.Thinning
    ( thinningPAct
    , breakingPAct
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
help = PAction $ \_ -> pure (Left (unlines ["thinning DELAY_IN_SECONDS", "breaking GAP_IN_SECONDS"]))

data Result
  = Result Img Int
  deriving Show
instance A.FromJSON Result where
  parseJSON =  A.withObject "Reslt" $ \v ->
    Result <$> v A..: "SourceFile" <*> v A..: "CreateDate"

runExiftool :: [Img] -> IO [Result]
runExiftool imgs = let
    mkArgs :: [Img] -> [String]
    mkArgs imgs = ["-quiet", "-dateformat", "%s", "-json", "-CreateDate"] ++ imgs
  in do
    (_, Just hout, _, pHandle) <- createProcess (proc "exiftool" (mkArgs imgs)) { std_out = CreatePipe }

    bs <- B.hGetContents hout

    exitCode <- waitForProcess pHandle
    unless (exitCode == ExitSuccess) $
      fail ("Exiftool failed with " ++ show exitCode)

    case (A.eitherDecode bs :: Either String [Result]) of
      Right results -> return results
      Left err      -> fail ("Failed to decode: " ++ err)

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
  results <- runExiftool imgs
  return (Right (thinningOnResults duration results))

thinningPAct :: PrePAction
thinningPAct ["-h"]     = help
thinningPAct [duration] = logSeparator ("Run thinning (with " ++ duration ++"s)") <> PAction (thinningImpl (read duration))
thinningPAct _          = help

breakingOnResults :: Int -> [Result] -> [Img]
breakingOnResults _        []     = []
breakingOnResults gap ((r@(Result img _)):rs) = let
    breakingOnResults' imgs' _ []                                            = imgs'
    breakingOnResults' imgs' (r1'@(Result _ time1)) ((r2'@(Result img' time2)):rs') =
      if (abs (time1 - time2)) > gap
        then imgs'
        else breakingOnResults' imgs' (r1') rs'
  in breakingOnResults' [img] r rs

breakingImpl :: Int -> [Img] -> PActionBody
breakingImpl gap imgs = do
  results <- runExiftool imgs
  return (Right (breakingOnResults gap results))

breakingPAct :: PrePAction
breakingPAct ["-h"] = help
breakingPAct [gap]  = logSeparator ("Run breaking (with " ++ gap ++"s)") <> PAction (breakingImpl (read gap))
breakingPAct _      = help
