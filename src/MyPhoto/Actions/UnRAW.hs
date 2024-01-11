module MyPhoto.Actions.UnRAW
  ( unRAW,
    unrawExtensions,
  )
where

import Control.Monad
import Data.Maybe (maybe)
import MyPhoto.Model
import MyPhoto.Wrapper.DcrawWrapper
import System.Exit
import System.FilePath
import System.Process

unrawExtensions :: [String]
unrawExtensions = [".arw", ".raw", ".nef"]

data ColorSpace
  = SRGBColorSpace
  | LinearColorSpace
  | DefaultColorSpace
  deriving (Show)

data WhiteBalanceOption
  = WBFromImage Img
  | WBFromFirstImage
  | WBFromRaw
  deriving (Show)

colorSpaceToArgs :: ColorSpace -> [String]
colorSpaceToArgs SRGBColorSpace = ["-6", "-g", "2.4", "12.92"]
colorSpaceToArgs LinearColorSpace = ["-w"]
colorSpaceToArgs DefaultColorSpace = []

data UnRawOptions = UnRawOptions
  { urVerbose :: Bool,
    urWhitebalance :: WhiteBalanceOption,
    urColorSpace :: ColorSpace,
    urQuality :: Int
  }
  deriving (Show)

instance Default UnRawOptions where
  def =
    UnRawOptions
      { urVerbose = False,
        urWhitebalance = WBFromRaw,
        urColorSpace = SRGBColorSpace,
        urQuality = 3
      }

calculateUnRAWedName :: FilePath -> FilePath
calculateUnRAWedName = (<.> "tiff")

getDcrawArgs :: UnRawOptions -> [Img] -> IO ([String], [Img])
getDcrawArgs opts =
  let addVerbosityArgs :: UnRawOptions -> ([String], [Img]) -> IO ([String], [Img])
      addVerbosityArgs UnRawOptions {urVerbose = True} (args, imgs) = pure (args ++ ["-v"], imgs)
      addVerbosityArgs _ v = pure v

      addWhiteBalanceArgs :: UnRawOptions -> ([String], [Img]) -> IO ([String], [Img])
      addWhiteBalanceArgs UnRawOptions {urWhitebalance = WBFromRaw} (args, imgs) = pure (args ++ ["-W"], imgs)
      addWhiteBalanceArgs UnRawOptions {urWhitebalance = WBFromImage wbImg} (args, imgs) = do
        multipliers <- calculateWhitebalance wbImg
        return (args ++ ["-r"] ++ multipliers, imgs)
      addWhiteBalanceArgs UnRawOptions {urWhitebalance = WBFromFirstImage} (args, wbImg : imgs) = addWhiteBalanceArgs (def {urWhitebalance = WBFromImage wbImg}) (args, imgs)
      addWhiteBalanceArgs UnRawOptions {urWhitebalance = WBFromFirstImage} (_, []) = fail "missing first image for whitebalance"

      addColorspaceArgs :: UnRawOptions -> ([String], [Img]) -> IO ([String], [Img])
      addColorspaceArgs UnRawOptions {urColorSpace = ocs} (args, imgs) = pure (args ++ colorSpaceToArgs ocs, imgs)

      addQualityArgs :: UnRawOptions -> ([String], [Img]) -> IO ([String], [Img])
      addQualityArgs UnRawOptions {urQuality = 3} v = pure v
      addQualityArgs UnRawOptions {urQuality = q} (args, imgs) = pure (args ++ ["-q", show q], imgs)

      addOutputFormaArgs :: ([String], [Img]) -> IO ([String], [Img])
      addOutputFormaArgs (args, imgs) = pure (args ++ ["-T"], imgs)
   in \imgs -> do
        (dcrawArgs, imgs') <-
          addVerbosityArgs opts ([], imgs)
            >>= addWhiteBalanceArgs opts
            >>= addColorspaceArgs opts
            >>= addQualityArgs opts
            >>= addOutputFormaArgs
        when (urVerbose opts) $ do
          putStrLn "dcrawArgs:"
          print dcrawArgs
          putStrLn "imgs:"
          print imgs'
        return (dcrawArgs, imgs')

unRAW :: UnRawOptions -> Imgs -> IO Imgs
unRAW opts imgs = do
  (dcrawArgs, imgs') <- getDcrawArgs opts imgs
  (exitCode, _, _) <- runDcraw (dcrawArgs ++ imgs')
  case exitCode of
    ExitSuccess -> return (map calculateUnRAWedName imgs') -- TODO: get generated output from "Writing data to <OUTPUT> ..." lines
    _ -> fail ("UnRAW failed with " ++ show exitCode)
