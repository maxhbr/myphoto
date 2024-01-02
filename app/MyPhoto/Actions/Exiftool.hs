{-# LANGUAGE OverloadedStrings #-}
module MyPhoto.Actions.Exiftool
    ( breaking
    ) where

import           Control.Monad
import           System.Process
import           System.Exit
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Lazy as B

import MyPhoto.Model hiding (Options(..))

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

breakingOnResults :: Int -> [Result] -> Imgs
breakingOnResults _        []     = []
breakingOnResults gapInSeconds ((r@(Result img _)):rs) = let
    breakingOnResults' imgs' _ []                                            = imgs'
    breakingOnResults' imgs' (r1'@(Result _ time1)) ((r2'@(Result img' time2)):rs') =
      if abs (time1 - time2) > gapInSeconds
        then imgs'
        else breakingOnResults' (imgs' ++ [img']) r2' rs'
  in breakingOnResults' [img] r rs

breaking :: Int -> Imgs -> IO Imgs
breaking gapInSeconds imgs = do
  results <- runExiftool imgs
  return (breakingOnResults gapInSeconds results)
