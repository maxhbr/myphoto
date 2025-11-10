module MyPhoto.Actions.Align
  ( align,
    AlignOptions (..),
    AlignNamingStrategy (..),
  )
where

import Control.Concurrent.Async (concurrently)
import Control.Monad
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import MyPhoto.Actions.Metadata (Metadata (..), getMetadataFromImgs)
import MyPhoto.Model
import System.Console.GetOpt
import System.Directory
import System.FilePath
import System.IO.Temp
import System.Process
import Text.Printf

data AlignNamingStrategy
  = AlignNamingStrategyOriginal
  | AlignNamingStrategySequential
  deriving (Eq, Show)

data AlignOptions = AlignOptions
  { alignOptVerbose :: Bool,
    alignOptNamingStrategy :: AlignNamingStrategy,
    alignOptSortBySize :: Bool
  }
  deriving (Eq, Show)

instance Default AlignOptions where
  def = AlignOptions False AlignNamingStrategySequential False

callAlignImageStack :: [String] -> String -> [Img] -> IO [Img]
callAlignImageStack alignArgs prefix imgs =
  let args = alignArgs ++ ["-a", prefix]
   in do
        putStrLn (unwords ["$ align_image_stack", unwords args, "[img [img [...]]]"])
        (_, _, _, pHandle) <- createProcess (proc "align_image_stack" (args ++ imgs))
        exitCode <- waitForProcess pHandle
        when (exitCode /= ExitSuccess) $
          fail ("callAlignImageStack failed with" ++ show exitCode)

        return (map (printf (prefix ++ "%04d.tif")) [0 .. (length imgs - 1)])

callAlignImageStackByHalves :: [String] -> FilePath -> [Img] -> IO [Img]
callAlignImageStackByHalves args tmpdir imgs =
  let imgsLen = length imgs
      firstImgs = take (imgsLen `div` 2 + 1) imgs
      lastImgs = drop (imgsLen `div` 2) imgs
   in if imgsLen < 4
        then callAlignImageStack args (tmpdir </> "fwd_") imgs
        else
          fmap (\(bwd, fwd) -> reverse (tail bwd) ++ fwd) $
            concurrently
              ( do
                  putStrLn ("align images from " ++ head firstImgs ++ " to " ++ last firstImgs ++ " (#=" ++ show (length firstImgs) ++ ")")
                  callAlignImageStack args (tmpdir </> "bwd_") (reverse firstImgs)
              )
              ( do
                  putStrLn ("align images from " ++ head lastImgs ++ " to " ++ last lastImgs ++ " (#=" ++ show (length lastImgs) ++ ")")
                  callAlignImageStack args (tmpdir </> "fwd_") lastImgs
              )

copyAndRenameImages :: (Int -> String) -> [Img] -> IO [Img]
copyAndRenameImages renamer imgs =
  mapM
    ( \(img, i) -> do
        let out = renamer i
        altOut <- findAltFileOfFile out
        copyFile img altOut
        return altOut
    )
    (zip imgs [1 ..])

getImageSize :: IO Img -> IO (Int, Int)
getImageSize ioImg = do
  img <- ioImg
  output <- readProcess "identify" ["-ping", "-format", "%w %h", img] ""
  let [width, height] = words output
  return (read width, read height)

getImagesSize :: Imgs -> IO [(Img, (Int, Int))]
getImagesSize imgs = mapM (\img -> do size <- getImageSize (return img); return (img, size)) imgs

growImage :: (Int, Int) -> FilePath -> Img -> IO (Img, Img)
growImage (targetW, targetH) wd img = do
  let (bn, ext) = splitExtensions img
      outImg = inWorkdir wd (bn ++ "_GROWN" ++ ext)
      geometryStr = printf "%dx%d" targetW targetH
  putStrLn ("growing " ++ img ++ " to " ++ geometryStr ++ " into " ++ outImg)
  (_, _, _, pHandle) <-
    createProcess
      ( proc
          "magick"
          [ img,
            "-background",
            "transparent",
            "-gravity",
            "center",
            "-extent",
            geometryStr,
            outImg
          ]
      )
  exitCode <- waitForProcess pHandle
  unless (exitCode == ExitSuccess) $
    fail ("growing image failed with " ++ show exitCode)
  return (img, outImg)

makeAllImagesTheSameSize :: AlignOptions -> FilePath -> Imgs -> IO [(Img, Img)]
makeAllImagesTheSameSize opts wd imgs =
  do
    imgsWithSize <- getImagesSize imgs
    let sizes = map snd imgsWithSize
        maxWidth = maximum (map fst sizes)
        maxHeight = maximum (map snd sizes)
        sorter =
          if alignOptSortBySize opts
            then sortBy (\(_, (w1, h1)) (_, (w2, h2)) -> compare (w2 * h2) (w1 * h1))
            else id
    if all (\(w, h) -> w == maxWidth && h == maxHeight) sizes
      then return (map (\img -> (img, img)) imgs)
      else
        mapM
          ( \(img, (w, h)) -> do
              if w == maxWidth && h == maxHeight
                then return (img, img)
                else growImage (maxWidth, maxHeight) wd img
          )
          (sorter imgsWithSize)

align :: AlignOptions -> FilePath -> Imgs -> IO Imgs
align _ _ [] = return []
align _ _ [img] = return [img]
align opts wd imgs = do
  let imgBN = computeStackOutputBN imgs
  let alignWD = wd </> imgBN <.> "align"
  createDirectoryIfMissing True alignWD

  -- TODO: look at: https://photo.stackexchange.com/a/83179
  let alignArgs =
        ["-v" | alignOptVerbose opts]
          ++ [ "--use-given-order",
               "-l", -- Assume linear input files
               "-c",
               "20", -- number of control points (per grid) to create between adjacent images
               "-s",
               "2", -- Scale down image by 2^scale (default: 1 [2x downsampling])
               -- , "-i" -- Optimize image center shift for all images, except for first.
               -- , "-m" -- Optimize field of view for all images, except for first. Useful for aligning focus stacks with slightly different magnification.
               "--gpu" -- Use GPU for remapping
             ]

  when (alignOptVerbose opts) $
    putStrLn $
      "aligning "
        ++ show (length imgs)
        ++ " images into working directory "
        ++ alignWD

  withTempDirectory
    alignWD
    ("_align_" ++ show (length imgs) ++ ".tmp")
    ( \tmpdir -> do
        createDirectoryIfMissing True tmpdir
        grownImgs <- makeAllImagesTheSameSize opts tmpdir imgs
        imgsInTmp <-
          if not (alignOptSortBySize opts)
            then callAlignImageStackByHalves alignArgs tmpdir (map snd grownImgs)
            else callAlignImageStack alignArgs (tmpdir </> "fwd_") (map snd grownImgs)

        imgsInTmp' <-
          concat
            <$> mapM
              ( \fn ->
                  let msg = "the file " ++ fn ++ " should exist after align"
                   in do
                        fnExists <- doesFileExist fn
                        if fnExists
                          then return [fn]
                          else do
                            putStrLn ("WARN: " ++ msg)
                            return []
              )
              imgsInTmp

        let bnAtIdx i = (dropExtension . fst) (grownImgs !! i)
            mkOutImgName :: Int -> String
            mkOutImgName i = case alignOptNamingStrategy opts of
              AlignNamingStrategyOriginal -> inWorkdir wd (bnAtIdx (i - 1) ++ "_ALIGNED.tif")
              AlignNamingStrategySequential -> inWorkdir alignWD (printf (bnAtIdx 0 ++ "_ALIGN-%04d-%04d.tif") i (length imgs))

        outs <- copyAndRenameImages mkOutImgName imgsInTmp'
        return outs
    )
