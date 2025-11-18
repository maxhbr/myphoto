module MyPhoto.Wrapper.FocusStackWrapper
  ( runFocusStack,
    FocusStackOptions (..),
    FocusStackCropping (..),
    computeAlignedImgs,
  )
where

import MyPhoto.Model
import System.Process

data FocusStackCropping
  = FocusStackCroppingDefault
  | FocusStackAlignKeepSize
  | FocusStackNoCrop
  deriving (Eq, Show)

data FocusStackOptions = FocusStackOptions
  { _verbose :: Bool,
    _depthMap :: Bool,
    _3DView :: Bool,
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
      _depthMap = depthMap,
      _3DView = d3DView,
      _cropping = cropping,
      _additionalParameters = additionalParameters,
      _output = output
    } =
    let verbosityOpt = ["--verbose" | verbose]
        croppingOpt = case cropping of
          FocusStackCroppingDefault -> []
          FocusStackAlignKeepSize -> ["--align-keep-size"]
          FocusStackNoCrop -> ["--nocrop"]
        outputOpt =
          ["--output=" ++ output]
            ++ (if depthMap then ["--depthmap=" ++ output ++ ".depthmap.png"] else [])
            ++ (if d3DView then ["--3dview=" ++ output ++ ".3dviewpt.png"] else [])
            ++ ["--save-steps", "--jpgquality=100", "--no-whitebalance", "--no-contrast"]
     in verbosityOpt ++ croppingOpt ++ outputOpt ++ additionalParameters

computeAlignedImgs :: FilePath -> Imgs -> [FilePath]
computeAlignedImgs workdir = map (\img -> workdir </> "aligned_" ++ takeFileName img)

runFocusStack :: FocusStackOptions -> IO (FilePath, [FilePath])
runFocusStack
  opts@FocusStackOptions
    { _imgs = imgs,
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
    outputExists <- doesFileExist output
    unless outputExists $ do
      fail $ "image not found: " ++ output

    let alignedImgs = computeAlignedImgs workdir imgs
    mapM_
      ( \img -> do
          imgExists <- doesFileExist img
          unless imgExists $ do
            fail $ "image not found: " ++ img
      )
      alignedImgs
    return (output, alignedImgs)
