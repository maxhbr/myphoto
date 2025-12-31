module MyPhoto.Actions.Align
  ( align,
    AlignOptions (..),
    AlignNamingStrategy (..),
    alignSmallerOnTopOfBigger,
    alignSmallerOnTopOfBiggest,
  )
where

import Control.Concurrent.Async (concurrently)
import Control.Monad
import Data.List (sortBy)
import MyPhoto.Actions.Metadata (getStackOutputBN)
import MyPhoto.Actions.UnTiff (unTiff)
import MyPhoto.Model
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
    alignOptSortBySize :: Bool,
    alignOptUntiff :: Bool
  }
  deriving (Eq, Show)

instance Default AlignOptions where
  def = AlignOptions False AlignNamingStrategySequential False True

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
  case words output of
    [width, height] -> return (read width, read height)
    _ -> fail ("unable to parse identify output: " ++ output)

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

makeAllImagesTheSameSize :: Bool -> FilePath -> Imgs -> IO [(Img, Img)]
makeAllImagesTheSameSize sortBySize wd imgs =
  do
    imgsWithSize <- getImagesSize imgs
    let sizes = map snd imgsWithSize
        maxWidth = maximum (map fst sizes)
        maxHeight = maximum (map snd sizes)
    if all (\(w, h) -> w == maxWidth && h == maxHeight) sizes
      then return (map (\img -> (img, img)) imgs)
      else do
        let sorter = if sortBySize
                        then sortBy (\(_, (w1, h1)) (_, (w2, h2)) -> compare (w2 * h2) (w1 * h1))
                        else id
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
  outputBN <- getStackOutputBN imgs
  let alignWD = wd </> outputBN <.> "align"
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
        grownImgs <- makeAllImagesTheSameSize (alignOptSortBySize opts) tmpdir imgs
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

        outTiffs <- copyAndRenameImages mkOutImgName imgsInTmp'
        if alignOptUntiff opts
          then unTiff True outTiffs
          else return outTiffs
    )

alignSmallerOnTopOfBigger :: FilePath -> Img -> Img -> IO Img
alignSmallerOnTopOfBigger wd bigImg smallImg = do
  out <- findAltFileOfFile (dropExtension smallImg ++ "_ALIGNED.tif")
  let alignWD = wd </> (takeFileName out) <.> "_alignSmallerOnTopOfBigger.tmp"
  createDirectoryIfMissing True alignWD
  (bigX, bigY) <- getImageSize (return bigImg)
  (smallX, smallY) <- getImageSize (return smallImg)
  when (bigX < smallX || bigY < smallY) $
    fail "alignSmallerOnTopOfBigger: first image must be bigger than second image"
  if bigX == smallX && bigY == smallY
                then do
                  putStrLn "both images are already the same size, no need to grow"
                  [_,aligned] <- callAlignImageStack ["-v", "--use-given-order"] "align_" [bigImg, smallImg]
                  copyFile aligned out
                else do
                  withTempDirectory
                    alignWD
                    ("_grow.tmp")
                    ( \tmpdir -> do
                        createDirectoryIfMissing True tmpdir
                        grownImgs <- makeAllImagesTheSameSize False tmpdir [bigImg, smallImg]
                        let alignArgs = ["-v", "--use-given-order"]
                        [_,aligned] <- callAlignImageStack alignArgs (tmpdir </> "align_") (map snd grownImgs)
                        copyFile aligned out
                    )
  return out

alignSmallerOnTopOfBiggest :: FilePath -> Imgs -> IO Imgs
alignSmallerOnTopOfBiggest _ [] = return []
alignSmallerOnTopOfBiggest _ [img] = return [img]
alignSmallerOnTopOfBiggest wd imgs = do
  imgsWithSize <- getImagesSize imgs
  let sizes = map snd imgsWithSize
      maxWidth = maximum (map fst sizes)
      maxHeight = maximum (map snd sizes)
      bigImage = head [img | (img, (w, h)) <- imgsWithSize, w == maxWidth, h == maxHeight]

  mapM ( \(img, (width, height)) -> 
          if img == bigImage
            then return img
            else if width == maxWidth && height == maxHeight
              then return img
              else do
                alignedImg <- alignSmallerOnTopOfBigger wd bigImage img
                return alignedImg
       ) imgsWithSize