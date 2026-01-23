{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MyPhoto.Actions.Gcp
  ( remoteProvisionScript,
    remoteExecuteScript,
    teardown,
    directDownload,
    runMain,
    setupBucket,
    uploadDockerTar,
    createVm,
    waitForSsh,
    provisionVm,
    cleanupOnProvisionFailure,
    runExecute,
    createDownloadScript,
    createDirectDownloadScript,
    createTeardownScript
  )
where

import Data.FileEmbed
import qualified Data.ByteString as BS
import MyPhoto.Model
import System.Process
import System.Exit
import System.Directory (doesFileExist)
import Data.Char
import Data.Time.Clock
import Data.Time.Format
import Data.Maybe (isNothing)
import Control.Exception (catch, SomeException)
import Control.Monad (forM_, unless)
import System.Directory (createDirectoryIfMissing)
import System.IO.Temp (withSystemTempFile)
import System.IO (hClose)

remoteProvisionScript :: BS.ByteString
remoteProvisionScript = $(embedFile "app-gcp/remote/provision.sh")

remoteExecuteScript :: BS.ByteString
remoteExecuteScript = $(embedFile "app-gcp/remote/execute.sh")

deleteVm :: GcpConfig -> String -> IO ()
deleteVm GcpConfig {..} vmName = do
  _ <- callProcess "gcloud"
    ["compute", "instances", "delete", vmName,
     "--project", gcpProject,
     "--zone", gcpZone,
     "--quiet"]
  return ()

deleteBucket :: String -> IO ()
deleteBucket bucket = do
  _ <- callProcess "gsutil" ["-m", "rm", "-r", bucket]
  return ()

deleteGcsObject :: String -> IO ()
deleteGcsObject path = do
  _ <- callProcess "gsutil" ["rm", path]
  return ()

runMain ::
  GcpConfig ->
  Maybe FilePath ->
  FilePath ->
  Maybe String ->
  Maybe FilePath ->
  Maybe String ->
  Maybe String ->
  Maybe String ->
  Maybe String ->
  Bool ->
  Bool ->
  Bool ->
  IO ()
runMain config@GcpConfig {..} inputDir outputDir inputBucket dockerImage
  machineType diskSize imageFamily imageProject
  keepVm keepBucket detach = do
  let outputBucket = "gs://myphoto-output/"
      dockerBucket = "gs://myphoto-docker-images/"
      dockerTarPath = dockerBucket ++ "myphoto-docker.tar"

  now <- getCurrentTime
  let date = formatTime defaultTimeLocale "%Y%m%d-%H%M%S" now
      sizeSuffix = case inputDir of
        Just _ -> "local"
        Nothing -> "gs"
      vmName = "myphoto-" ++ date ++ "-" ++ sizeSuffix
      labelValue = normalizeLabel vmName

  actualInputBucket <- case inputBucket of
    Just bucket -> return bucket
    Nothing -> do
      let bucketName = "gs://" ++ vmName ++ "-input/"
      setupBucket config bucketName labelValue date inputDir
      return bucketName

  setupBucketIfNotExists config outputBucket labelValue date Nothing True

  case dockerImage of
    Just dockerImage' -> uploadDockerTar config dockerTarPath dockerImage'
    Nothing -> return ()

  let maybeDockerTarForCleanup = dockerImage
      machineType' = case machineType of
        Just mt -> mt
        Nothing -> "n2-standard-32"
      diskSize' = case diskSize of
        Just ds -> ds
        Nothing -> "500GB"
      imageFamily' = case imageFamily of
        Just ifam -> ifam
        Nothing -> "debian-12"
      imageProject' = case imageProject of
        Just iproj -> iproj
        Nothing -> "debian-cloud"

  createVm config vmName machineType' diskSize' imageFamily' imageProject' labelValue date

  waitForSsh config vmName (Just actualInputBucket) (if isNothing maybeDockerTarForCleanup then Nothing else Just dockerTarPath)

  executeScript <- provisionVm config vmName (Just actualInputBucket) (if isNothing maybeDockerTarForCleanup then Nothing else Just dockerTarPath)

  let outputBucketPath = outputBucket ++ vmName ++ "/"

  createDirectoryIfMissing True outputDir
  createDownloadScript outputDir vmName config outputBucketPath
  createDirectDownloadScript outputDir vmName
  createTeardownScript outputDir vmName actualInputBucket

  runExecute config vmName actualInputBucket outputBucketPath dockerTarPath keepVm keepBucket detach executeScript

  where 
    normalizeLabel :: String -> String
    normalizeLabel = map (\c -> if isAlphaNum c || c `elem` ['_', '-'] then toLower c else '-')
      . takeWhile (/= '\0')

teardown :: String -> Maybe String -> GcpConfig -> IO ()
teardown vmName maybeInputBucket config = do
  deleteVm config vmName
  forM_ maybeInputBucket $ \bucket -> deleteBucket bucket

directDownload :: String -> Maybe FilePath -> GcpConfig -> IO ()
directDownload vmName maybeOutputDir GcpConfig {..} = do
  let zone = gcpZone
      project = gcpProject
      outputDir = case maybeOutputDir of
        Just d -> d
        Nothing -> "."
  
  checkResult <- readProcess "gcloud"
    ["compute", "instances", "describe", vmName,
     "--project", project,
     "--zone", zone,
     "--format", "get(status)"] ""
  
  unless (checkResult `elem` ["RUNNING\n", "RUNNING"]) $ do
    putStrLn $ "Error: VM " ++ vmName ++ " is no longer running"
    exitWith (ExitFailure 1)
  
  _ <- callProcess "gcloud"
    ["compute", "scp", "--recurse", vmName ++ ":/data/workdir/*", outputDir,
     "--project", project,
     "--zone", zone]
  return ()

setupBucket :: GcpConfig -> String -> String -> String -> Maybe FilePath -> IO ()
setupBucket GcpConfig {..} bucketName labelValue date initialContent = do
  _ <- callProcess "gsutil" ["mb", "-p", gcpProject, "-l", gcpRegion, bucketName]
  
  _ <- callProcess "gcloud" 
    ["storage", "buckets", "update", "--clear-soft-delete", bucketName]
  
  case initialContent of
    Just dir -> do
      _ <- callProcess "gcloud" 
        ["storage", "buckets", "update", bucketName,
         "--project", gcpProject,
         "--update-labels", "run=" ++ labelValue ++ ",date=" ++ date]
      _ <- callProcess "gsutil" ["-m", "rsync", "-r", dir, bucketName]
      return ()
    Nothing -> return ()

setupBucketIfNotExists :: GcpConfig -> String -> String -> String -> Maybe FilePath -> Bool -> IO ()
setupBucketIfNotExists config@GcpConfig {..} bucketName labelValue date initialContent skipIfExists = do
  if skipIfExists
    then do
      bucketExists <- catch (readProcess "gsutil" ["ls", bucketName] "" >> return True) 
                           (\(_ :: SomeException) -> return False)
      if not bucketExists
        then setupBucket config bucketName labelValue date initialContent
        else putStrLn $ "Bucket already exists: " ++ bucketName
    else setupBucket config bucketName labelValue date initialContent

uploadDockerTar :: GcpConfig -> String -> FilePath -> IO ()
uploadDockerTar _ dockerTarPath dockerImage = do 
  exists <- doesFileExist dockerImage
  unless exists $ do
    putStrLn $ "Docker tar file not found: " ++ dockerImage
    exitWith (ExitFailure 1)
  putStrLn $ "Uploading Docker image to " ++ dockerTarPath
  _ <- callProcess "gsutil" ["cp", dockerImage, dockerTarPath]
  return ()

createVm :: GcpConfig -> String -> String -> String -> String -> String -> String -> String -> IO ()
createVm GcpConfig {..} vmName machineType diskSize imageFamily imageProject labelValue date = do
  let scopes = "https://www.googleapis.com/auth/cloud-platform"
  _ <- callProcess "gcloud"
    ["compute", "instances", "create", vmName,
     "--project", gcpProject,
     "--zone", gcpZone,
     "--machine-type", machineType,
     "--boot-disk-size", diskSize,
     "--image-family", imageFamily,
     "--image-project", imageProject,
     "--labels", "run=" ++ labelValue ++ ",date=" ++ date,
     "--scopes", scopes]
  return ()

waitForSsh :: GcpConfig -> String -> Maybe String -> Maybe String -> IO ()
waitForSsh GcpConfig {..} vmName inputBucket dockerTarPath = do
  let maxAttempts = 10
      sleepSeconds = 5
      attempt 0 = do
        putStrLn $ "SSH did not become available on " ++ vmName
        cleanupOnProvisionFailureWithPath GcpConfig{..} vmName inputBucket dockerTarPath
      attempt n = do
        exitCode <- catch
          (callProcess "gcloud"
            ["compute", "ssh", vmName,
             "--project", gcpProject,
             "--zone", gcpZone,
             "--command", "true",
             "--ssh-flag=-o ConnectionAttempts=3",
             "--ssh-flag=-o ConnectTimeout=" ++ show sleepSeconds,
             "--ssh-flag=-o StrictHostKeyChecking=no"] >> return ExitSuccess)
          (\(_ :: SomeException) -> return (ExitFailure 1))
        case exitCode of
          ExitSuccess -> return ()
          _ -> do
            putStrLn $ "Waiting for SSH on " ++ vmName ++ " (attempt " ++ show (maxAttempts - n + 1) ++ "/" ++ show maxAttempts ++ ")..."
            _ <- system $ "sleep " ++ show sleepSeconds
            attempt (n - 1)
  attempt maxAttempts

uploadScripts :: GcpConfig -> String -> IO (String,String)
uploadScripts GcpConfig {..} vmName = do
  let provisionScript = "~/myphoto-remote-provision.sh"
      executeScript = "~/myphoto-remote-execute.sh"
  mapM_ (\(content, remotePath) ->
    withSystemTempFile "myphoto-script.sh" $ \localPath handle -> do
      BS.hPut handle content
      hClose handle
      callProcess "gcloud"
        ["compute", "scp", localPath, vmName ++ ":" ++ remotePath,
         "--project", gcpProject,
         "--zone", gcpZone])
    [ (remoteProvisionScript, provisionScript)
    , (remoteExecuteScript, executeScript)
    ]
  return (provisionScript, executeScript) 

provisionVm :: GcpConfig -> String -> Maybe String -> Maybe String -> IO String
provisionVm GcpConfig {..} vmName inputBucket dockerTarPath = do
  (provisionScript, executeScript) <- uploadScripts GcpConfig{..} vmName
  exitCode <- catch
    (callProcess "gcloud"
      ["compute", "ssh", vmName,
       "--project", gcpProject,
       "--zone", gcpZone,
       "--command", "bash " ++ provisionScript] >> return ExitSuccess)
    (\(_ :: SomeException) -> return (ExitFailure 1))

  case exitCode of
    ExitSuccess -> return executeScript
    _ -> do
      putStrLn "Provisioning failed, cleaning up..."
      cleanupOnProvisionFailureWithPath (GcpConfig{..}) vmName inputBucket dockerTarPath

cleanupOnProvisionFailure :: String -> Maybe String -> IO ()
cleanupOnProvisionFailure vmName dockerTarPath = do
  return ()
  
cleanupOnProvisionFailureWithPath :: GcpConfig -> String -> Maybe String -> Maybe String -> IO a
cleanupOnProvisionFailureWithPath config@GcpConfig {..} vmName inputBucket dockerTarPath = do
  putStrLn $ "Cleaning up after provisioning failure..."
  deleteVm config vmName
  forM_ inputBucket $ \bucket -> deleteBucket bucket
  forM_ dockerTarPath $ \tarPath -> deleteGcsObject tarPath
  exitWith (ExitFailure 1)

runExecute ::
  GcpConfig ->
  String ->
  String ->
  String ->
  String ->
  Bool ->
  Bool ->
  Bool ->
  String ->
  IO ()
runExecute config@GcpConfig {..} vmName inputBucket outputBucketPath dockerTarPath keepVm keepBucket detach executeScript = do
  if detach
    then do
      _ <- callProcess "gcloud"
        ["compute", "ssh", vmName,
         "--project", gcpProject,
         "--zone", gcpZone,
         "--command", 
          "nohup bash -c \\\"bash " ++ executeScript ++ " '" ++ inputBucket ++ 
           "' '" ++ outputBucketPath ++ "' '" ++ dockerTarPath ++ "' no\\\" > /dev/null 2>&1 &"]
      putStrLn "Detached execution started."
      putStrLn $ "Output will be available at: " ++ outputBucketPath
    else do
      _ <- callProcess "gcloud"
        ["compute", "ssh", vmName,
         "--project", gcpProject,
         "--zone", gcpZone,
         "--command",
         "bash ~/myphoto-remote-execute.sh '" ++ inputBucket ++ 
          "' '" ++ outputBucketPath ++ "' '" ++ dockerTarPath ++ "' no"]
      
      unless keepVm $ do
        putStrLn $ "cleaning up VM: " ++ vmName
        deleteVm config vmName
      
      unless keepBucket $ do
        putStrLn $ "cleaning up input bucket: " ++ inputBucket
        deleteBucket inputBucket
      
      _ <- callProcess "gsutil" ["-m", "rsync", "-r", outputBucketPath, "."]
      return ()

createDownloadScript :: FilePath -> String -> GcpConfig -> String -> IO ()
createDownloadScript outputDir vmName GcpConfig {..} outputBucketPath = do
  let scriptPath = outputDir ++ "/" ++ vmName ++ ".download.sh"
      scriptContent = unlines
        [ "#!/usr/bin/env bash"
        , "set -euo pipefail"
        , ""
        , "VM_NAME=\"" ++ vmName ++ "\""
        , "PROJECT=\"" ++ gcpProject ++ "\""
        , "ZONE=\"" ++ gcpZone ++ "\""
        , "OUTPUT_DIR=\"" ++ outputDir ++ "\""
        , "OUTPUT_BUCKET_PATH=\"" ++ outputBucketPath ++ "\""
        , ""
        , "if gcloud compute instances describe \"$VM_NAME\" \\"
        , "    --project=\"$PROJECT\" \\"
        , "    --zone=\"$ZONE\" \\"
        , "    --format=\"get(status)\" 2>/dev/null | grep -q \"RUNNING\"; then"
        , "  echo \"Error: VM $VM_NAME is still running. Wait for the job to complete before downloading.\""
        , "  exit 1"
        , "fi"
        , ""
        , "mkdir -p \"$OUTPUT_DIR\""
        , "set -x"
        , "gsutil -m rsync -r \"$OUTPUT_BUCKET_PATH\" \"$OUTPUT_DIR\""
        ]
  writeFile scriptPath scriptContent
  _ <- callProcess "chmod" ["+x", scriptPath]
  putStrLn $ "Download script created: " ++ scriptPath

createDirectDownloadScript :: FilePath -> String -> IO ()
createDirectDownloadScript outputDir vmName = do
  let scriptPath = outputDir ++ "/" ++ vmName ++ ".direct-download.sh"
      scriptContent = unlines
        [ "#!/usr/bin/env bash"
        , "set -euo pipefail"
        , ""
        , "set -x"
        , "exec \"$0\" --direct-download \"" ++ vmName ++ "\" \"" ++ outputDir ++ "\""
        ]
  writeFile scriptPath scriptContent
  _ <- callProcess "chmod" ["+x", scriptPath]
  return ()

createTeardownScript :: FilePath -> String -> String -> IO ()
createTeardownScript outputDir vmName inputBucket = do
  let scriptPath = outputDir ++ "/" ++ vmName ++ ".teardown.sh"
      scriptContent = unlines
        [ "#!/usr/bin/env bash"
        , "set -euo pipefail"
        , ""
        , "set -x"
        , "exec \"$0\" --teardown \"" ++ vmName ++ "\" \"" ++ inputBucket ++ "\""
        ]
  writeFile scriptPath scriptContent
  _ <- callProcess "chmod" ["+x", scriptPath]
  return ()