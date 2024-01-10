module MyPhoto.Wrapper.FocusStack
  ( runFocusStack,
    FocusStackOptions (..),
    FocusStackCropping (..),
    computeAlignedImgs,
  )
where

import qualified Data.Map as Map
import MyPhoto.Model
import qualified System.IO as IO
import System.Process

data FocusStackCropping
  = FocusStackCroppingDefault
  | FocusStackAlignKeepSize
  | FocusStackNoCrop
  deriving (Eq, Show)

data FocusStackOptions = FocusStackOptions
  { _verbose :: Bool,
    _cropping :: FocusStackCropping,
    _additionalParameters :: [String],
    _imgs :: Imgs,
    _workdir :: FilePath,
    _output :: Img
  }
  deriving (Show)

focusStackOptionsToArgs :: FocusStackOptions -> [String]
focusStackOptionsToArgs
  FocusStackOptions
    { _verbose = verbose,
      _cropping = cropping,
      _additionalParameters = additionalParameters,
      _output = output
    } =
    let verbosityOpt = ["--verbose" | verbose]
        croppingOpt = case cropping of
          FocusStackCroppingDefault -> []
          FocusStackAlignKeepSize -> ["--align-keep-size"]
          FocusStackNoCrop -> ["--nocrop"]
        outputOpt = ["--output=" ++ output, "--save-steps", "--jpgquality=100"]
     in -- ("--depthmap=" ++ output ++ ".depthmap.png"),
        -- ("--3dview=" ++ output ++ ".3dviewpt.png"),
        verbosityOpt ++ croppingOpt ++ outputOpt ++ additionalParameters

computeAlignedImgs :: FilePath -> Imgs -> [FilePath]
computeAlignedImgs workdir = map (\img -> workdir </> "aligned_" ++ takeFileName img)

runFocusStack :: FocusStackOptions -> IO (FilePath, [FilePath])
runFocusStack
  opts@FocusStackOptions
    { _verbose = verbose,
      _cropping = cropping,
      _additionalParameters = additionalParameters,
      _imgs = imgs,
      _workdir = workdir,
      _output = output
    } = do
    let args = focusStackOptionsToArgs opts
    createDirectoryIfMissing True workdir
    logDebugIO (unwords ["$ focus-stack", unwords args, "[img [img [...]]]"])
    (_, _, _, ph) <-
      createProcess
        ( proc
            "focus-stack"
            (args ++ imgs)
        )
          { cwd = Just workdir
          }
    exitcode <- waitForProcess ph
    when (exitcode /= ExitSuccess) $ do
      fail $ "ERR: focus-stack exited with " ++ (show exitcode)
    exists <- doesFileExist output
    unless exists $ do
      fail $ "image not found: " ++ output

    let alignedImgs = computeAlignedImgs workdir imgs
    mapM_
      ( \img -> do
          exists <- doesFileExist img
          unless exists $ do
            fail $ "image not found: " ++ img
      )
      alignedImgs
    return (output, alignedImgs)
