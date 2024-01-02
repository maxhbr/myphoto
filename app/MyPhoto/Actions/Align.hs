module MyPhoto.Actions.Align
  ( align
  ) where

import           System.FilePath
import           System.Console.GetOpt
import           Control.Monad
import           Data.Maybe (fromMaybe)
import           System.Process
import           System.Exit
import           System.Directory
import           System.IO.Temp
import           Text.Printf
import           Control.Concurrent.Async (concurrently)

import MyPhoto.Model


callAlignImageStack :: [String] -> String -> [Img] -> IO [Img]
callAlignImageStack alignArgs prefix imgs = let
    args = alignArgs ++ ["-a", prefix]
  in do
    putStrLn (unwords ["$ align_image_stack", unwords args, "[img [img [...]]]"])
    (_, _, _, pHandle) <- createProcess (proc "align_image_stack" (args ++ imgs))
    exitCode <- waitForProcess pHandle
    when (exitCode /= ExitSuccess) $
      fail ("callAlignImageStack failed with" ++ show exitCode)

    return (map (printf (prefix ++ "%04d.tif")) [0..(length imgs - 1)])

callAlignImageStackByHalves :: [String] -> FilePath -> [Img] -> IO [Img]
callAlignImageStackByHalves args tmpdir imgs = let
    imgsLen   = length imgs
    firstImgs = take (imgsLen `div` 2 + 1) imgs
    lastImgs  = drop (imgsLen `div` 2) imgs
  in if imgsLen < 4
     then callAlignImageStack args (tmpdir </> "fwd_") imgs
     else fmap (\(bwd, fwd) -> reverse (tail bwd) ++ fwd) $ concurrently
        ( do
            putStrLn ("align images from " ++ head firstImgs ++ " to " ++ last firstImgs ++ " (#=" ++ show (length firstImgs) ++ ")")
            callAlignImageStack args (tmpdir </> "bwd_") (reverse firstImgs)
        )
        ( do
            putStrLn ("align images from " ++ head lastImgs ++ " to " ++ last lastImgs ++ " (#=" ++ show (length lastImgs) ++ ")")
            callAlignImageStack args  (tmpdir </> "fwd_") lastImgs
        )

copyAndRenameImages :: (Int -> String) -> [Img] -> IO [Img]
copyAndRenameImages renamer imgs = mapM (\(img, i) -> do
                                            let out = renamer i
                                            altOut <- findAltFileOfFile out
                                            copyFile img altOut
                                            return altOut) (zip imgs [1..])

align :: Options -> Imgs -> IO Imgs
align _ [] = return []
align opts@Options{optWorkdir = Nothing} imgs@(img1:_) = align opts{optWorkdir = Just (takeDirectory img1)} imgs
align opts@Options{optWorkdir = Just wd} imgs = do
  let alignWD = wd </> "_align"
  createDirectoryIfMissing True alignWD

  -- TODO: look at: https://photo.stackexchange.com/a/83179
  let alignArgs = [ "-v" | optVerbose opts ]
               ++ [ "--use-given-order"
                 , "-l" -- Assume linear input files
                 , "-c", "20" -- number of control points (per grid) to create between adjacent images
                 , "-s", "2"  -- Scale down image by 2^scale (default: 1 [2x downsampling])
                 -- , "-i" -- Optimize image center shift for all images, except for first.
                 -- , "-m" -- Optimize field of view for all images, except for first. Useful for aligning focus stacks with slightly different magnification.
                 , "--gpu" -- Use GPU for remapping
                 ]

  let prefix = dropExtension (head imgs)
      mkOutImgName :: Int -> String
      mkOutImgName i = inWorkdir' alignWD (printf (prefix ++ "_ALIGN-%04d-%04d.tif") i (length imgs))

  withTempDirectory wd ("_align_" ++ show (length imgs) ++ ".tmp")
    (\tmpdir -> do
        imgsInTmp <- callAlignImageStackByHalves alignArgs tmpdir imgs

        imgsInTmp' <- concat <$> mapM (
          \fn -> let
            msg = "the file " ++ fn ++ " should exist after align"
          in do
            fnExists <- doesFileExist fn
            if fnExists
              then return [fn]
              else do
                putStrLn ("WARN: " ++ msg)
                return []
            ) imgsInTmp

        outs <- copyAndRenameImages mkOutImgName imgsInTmp'
        return outs
      )
