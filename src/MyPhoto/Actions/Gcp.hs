{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module MyPhoto.Actions.Gcp
  ( remoteProvisionScript,
    remoteExecuteScript,
    teardown,
    directDownload
  )
where

import Data.FileEmbed
import qualified Data.ByteString as BS
import MyPhoto.Model

remoteProvisionScript :: BS.ByteString
remoteProvisionScript = $(embedFile "app-gcp/remote/provision.sh")

remoteExecuteScript :: BS.ByteString
remoteExecuteScript = $(embedFile "app-gcp/remote/execute.sh")

teardown :: String -> Maybe String -> GcpConfig -> IO ()
teardown vmName _ _ = do
  putStrLn $ "Teardown called for: " ++ vmName
  return ()

directDownload :: String -> Maybe FilePath -> GcpConfig -> IO ()
directDownload vmName _ _ = do
  putStrLn $ "Direct download called for: " ++ vmName
  return ()