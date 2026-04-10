{-# LANGUAGE ScopedTypeVariables #-}

module MyPhoto.Actions.Align
  ( align,
    AlignOptions (..),
    AlignNamingStrategy (..),
    alignSmallerOnTopOfBigger,
    alignSmallerOnTopOfBiggest,
    cropToCommonIntersection,
    parseCompareOffset,
  )
where

import Control.Concurrent.Async (concurrently)
import Control.Exception (SomeException, catch)
import Control.Monad
import Data.Char (isSpace)
import Data.List (sortBy)
import MyPhoto.Actions.Metadata (getStackOutputBN)
import MyPhoto.Actions.UnTiff (unTiff)
import MyPhoto.Model
import System.Directory
import System.FilePath
import System.IO (hGetContents')
import System.IO.Temp
import System.Process
import Text.Printf
import Text.Read (readMaybe)

data AlignNamingStrategy
  = AlignNamingStrategyOriginal
  | AlignNamingStrategySequential
  deriving (Eq, Show)

data AlignOptions = AlignOptions
  { alignOptVerbose :: Bool,
    alignOptNamingStrategy :: AlignNamingStrategy,
    alignOptSortBySize :: Bool,
    alignOptUntiff :: Bool,
    alignOptNoGpu :: Bool
  }
  deriving (Eq, Show)

instance Default AlignOptions where
  def = AlignOptions False AlignNamingStrategySequential False True False

callAlignImageStack :: [String] -> String -> [Img] -> IO [Img]
callAlignImageStack alignArgs prefix imgs =
  let args = alignArgs ++ ["-a", prefix]
   in do
        logDebugIO (unwords ["$ align_image_stack", unwords args, "[img [img [...]]]"])
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
                  logInfoIO ("align images from " ++ head firstImgs ++ " to " ++ last firstImgs ++ " (#=" ++ show (length firstImgs) ++ ")")
                  callAlignImageStack args (tmpdir </> "bwd_") (reverse firstImgs)
              )
              ( do
                  logInfoIO ("align images from " ++ head lastImgs ++ " to " ++ last lastImgs ++ " (#=" ++ show (length lastImgs) ++ ")")
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
  logInfoIO ("growing " ++ img ++ " to " ++ geometryStr ++ " into " ++ outImg)
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
        let sorter =
              if sortBySize
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
               "2" -- Scale down image by 2^scale (default: 1 [2x downsampling])
               -- , "-i" -- Optimize image center shift for all images, except for first.
               -- , "-m" -- Optimize field of view for all images, except for first. Useful for aligning focus stacks with slightly different magnification.
             ]
          ++ ["--gpu" | not (alignOptNoGpu opts)]

  when (alignOptVerbose opts) $
    logInfoIO $
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
                            logWarnIO msg
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

-- | Find the integer (x, y) translation offset of a target image relative to
-- a reference image using ImageMagick's subimage search with NCC metric.
-- Both images are downscaled for speed; the offset is scaled back up.
-- Returns (dx, dy) where the target's content starts at (dx, dy) in the reference.
findTranslationOffset :: FilePath -> Img -> Img -> IO (Int, Int)
findTranslationOffset tmpdir refImg targetImg = do
  let scaleFactor = 4 :: Int
      refSmall = tmpdir </> "ref_small.png"
      targetSmall = tmpdir </> "target_small.png"
      pctStr = show (100 `div` scaleFactor) ++ "%"
  -- Downscale both images in parallel for fast correlation
  logDebugIO ("downscaling images to " ++ pctStr ++ " for offset detection")
  (ec1, ec2) <-
    concurrently
      ( do
          (_, _, _, ph1) <- createProcess (proc "magick" [refImg, "-resize", pctStr, refSmall])
          waitForProcess ph1
      )
      ( do
          (_, _, _, ph2) <- createProcess (proc "magick" [targetImg, "-resize", pctStr, targetSmall])
          waitForProcess ph2
      )
  unless (ec1 == ExitSuccess) $
    fail "downscaling reference image failed"
  unless (ec2 == ExitSuccess) $
    fail "downscaling target image failed"
  -- Use subimage-search to find the best translation offset.
  -- magick compare writes metrics to stderr and exits 1 for "dissimilar" images,
  -- which is expected — we only need the offset, not the similarity score.
  logDebugIO "running subimage search for translation offset"
  (_, _, Just herr, ph) <-
    createProcess
      (proc "magick" ["compare", "-metric", "NCC", "-subimage-search", refSmall, targetSmall, "null:"])
        { std_err = CreatePipe
        }
  errOutput <- hGetContents' herr
  _ <- waitForProcess ph -- ignore exit code; compare returns 1 for dissimilar
  logDebugIO ("compare stderr: " ++ errOutput)
  -- Parse output format: "score (normalized) @ X,Y [similarity]"
  (dx, dy) <- case parseCompareOffset errOutput of
    Just (x, y) -> return (x, y)
    Nothing -> do
      logWarnIO ("unable to parse offset from compare output, assuming (0,0): " ++ errOutput)
      return (0, 0)
  let fullDx = dx * scaleFactor
      fullDy = dy * scaleFactor
  logInfoIO ("translation offset: dx=" ++ show fullDx ++ " dy=" ++ show fullDy ++ " (from downscaled " ++ show dx ++ "," ++ show dy ++ ")")
  return (fullDx, fullDy)

-- | Parse the "@ X,Y" offset from magick compare output.
parseCompareOffset :: String -> Maybe (Int, Int)
parseCompareOffset s = case break (== '@') s of
  (_, '@' : rest) ->
    let trimmed = dropWhile isSpace rest
        (xStr, afterX) = break (\c -> c == ',' || isSpace c) trimmed
        afterComma = drop 1 afterX
        (yStr, _) = break (\c -> isSpace c || c == '[') (dropWhile isSpace afterComma)
     in case (reads xStr, reads yStr) of
          ([(x, "")], [(y, "")]) -> Just (x, y)
          _ -> Nothing
  _ -> Nothing

alignSmallerOnTopOfBigger :: FilePath -> Img -> Img -> IO Img
alignSmallerOnTopOfBigger wd bigImg smallImg = do
  out <- findAltFileOfFile (dropExtension smallImg ++ "_ALIGNED.tif")
  let alignWD = wd </> (takeFileName out) <.> "_alignSmallerOnTopOfBigger.tmp"
  createDirectoryIfMissing True alignWD
  (bigX, bigY) <- getImageSize (return bigImg)
  (smallX, smallY) <- getImageSize (return smallImg)
  when (bigX < smallX || bigY < smallY) $
    fail "alignSmallerOnTopOfBigger: first image must be bigger than second image"
  withTempDirectory
    alignWD
    "_translate.tmp"
    ( \tmpdir -> do
        createDirectoryIfMissing True tmpdir
        -- Grow the smaller image to the same canvas size as the bigger one
        -- (centered, with transparent padding)
        grownSmall <-
          if bigX == smallX && bigY == smallY
            then return smallImg
            else snd <$> growImage (bigX, bigY) tmpdir smallImg
        -- Find the translation offset
        (dx, dy) <- findTranslationOffset tmpdir bigImg grownSmall
        -- Apply the offset: shift the grown image by (dx, dy) by setting
        -- its virtual canvas position and using -flatten to composite.
        -- This is pure pixel copying — no resampling or interpolation.
        logInfoIO
          ( "aligning "
              ++ smallImg
              ++ " onto "
              ++ bigImg
              ++ " with translation ("
              ++ show dx
              ++ ","
              ++ show dy
              ++ ")"
          )
        -- Create the shifted output by adjusting the virtual canvas position of the
        -- grown image and extending/cropping to match the reference dimensions.
        -- This is pure pixel copying — no resampling or interpolation.
        --
        -- Steps:
        -- 1. Set the page/offset of the grown image to (dx, dy)
        -- 2. Use -set page to define the full canvas
        -- 3. Use -flatten with transparent background to composite onto a canvas
        --    of the reference dimensions, preserving alpha.
        (_, _, _, pH) <-
          createProcess
            ( proc
                "magick"
                [ grownSmall,
                  "-alpha",
                  "on",
                  "-repage",
                  printf "%dx%d%+d%+d" bigX bigY dx dy,
                  "-background",
                  "transparent",
                  "-flatten",
                  out
                ]
            )
        ec <- waitForProcess pH
        unless (ec == ExitSuccess) $
          fail ("translation alignment failed with " ++ show ec)
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

  mapM
    ( \(img, (width, height)) ->
        if img == bigImage
          then return img
          else
            if width == maxWidth && height == maxHeight
              then return img
              else do
                catch
                  (alignSmallerOnTopOfBigger wd bigImage img)
                  ( \(e :: SomeException) -> do
                      logWarnIO ("alignment failed for " ++ img ++ ": " ++ show e)
                      return img
                  )
    )
    imgsWithSize

-- | Format a WxH+X+Y geometry string for ImageMagick.
showGeom :: Int -> Int -> Int -> Int -> String
showGeom w h x y = show w ++ "x" ++ show h ++ "+" ++ show x ++ "+" ++ show y

-- | Check whether an entire rectangle in the mask image is fully opaque.
-- Uses ImageMagick to crop to the rectangle and check the minimum pixel value.
isRectOpaque :: FilePath -> Int -> Int -> Int -> Int -> IO Bool
isRectOpaque maskFile x y w h = do
  let geom = showGeom w h x y
  output <-
    readProcess
      "magick"
      [maskFile, "-crop", geom, "+repage", "-format", "%[min]", "info:"]
      ""
  case readMaybe (filter (not . isSpace) output) :: Maybe Int of
    Just val -> return (val == 255 || val == 65535)
    Nothing -> do
      logWarnIO ("isRectOpaque: unexpected magick output: " ++ show output)
      return False

-- | Binary search for the minimum value in [lo, hi) where the predicate is True.
-- Assumes monotonic transition from False to True.
-- Returns Nothing if the predicate is False for all values.
bsearchFirst :: (Int -> IO Bool) -> Int -> Int -> IO (Maybe Int)
bsearchFirst predicate lo hi
  | lo >= hi = return Nothing
  | otherwise = do
      lastVal <- predicate (hi - 1)
      if not lastVal
        then return Nothing
        else go lo (hi - 1)
  where
    go low high
      | low >= high = return (Just low)
      | otherwise = do
          let mid = low + (high - low) `div` 2
          val <- predicate mid
          if val
            then go low mid
            else go (mid + 1) high

-- | Binary search for the maximum value in [lo, hi) where the predicate is True.
-- Assumes monotonic transition from True to False.
-- Returns Nothing if the predicate is False for all values.
bsearchLast :: (Int -> IO Bool) -> Int -> Int -> IO (Maybe Int)
bsearchLast predicate lo hi
  | lo >= hi = return Nothing
  | otherwise = do
      firstVal <- predicate lo
      if not firstVal
        then return Nothing
        else go lo (hi - 1)
  where
    go low high
      | low >= high = return (Just low)
      | otherwise = do
          let mid = low + (high - low + 1) `div` 2
          val <- predicate mid
          if val
            then go mid high
            else go low (mid - 1)

-- | Get the bounding box of non-black content via ImageMagick -trim.
-- Returns (width, height, xOffset, yOffset) or Nothing on failure.
getTrimBoundingBox :: FilePath -> IO (Maybe (Int, Int, Int, Int))
getTrimBoundingBox img = do
  output <-
    readProcess
      "magick"
      [img, "-trim", "-print", "%w %h %X %Y\\n", "null:"]
      ""
  let safeRead s = readMaybe s :: Maybe Int
      parseOffset ('+' : s) = safeRead s
      parseOffset s = safeRead s
  case lines output of
    [] -> do
      logWarnIO ("unable to parse trim bounding box: empty magick output")
      return Nothing
    (firstLine : _) -> case words firstLine of
      [wS, hS, xS, yS] ->
        case (safeRead wS, safeRead hS, parseOffset xS, parseOffset yS) of
          (Just w, Just h, Just x, Just y) -> return $ Just (w, h, x, y)
          _ -> do
            logWarnIO ("unable to parse trim bounding box values: " ++ firstLine)
            return Nothing
      _ -> do
        logWarnIO ("unable to parse trim bounding box from magick output: " ++ output)
        return Nothing

-- | Find the largest fully-opaque crop rectangle inside the given binary mask.
--
-- Algorithm:
-- 1. Find an initial opaque rectangle by shaving all edges uniformly
--    (binary search on the shave percentage).
-- 2. Expand each edge outward as far as possible while keeping the
--    rectangle fully opaque.
-- 3. Iterate until stable, producing the largest axis-aligned rectangle
--    that is fully opaque.
--
-- Each candidate rectangle is checked with 'isRectOpaque' (full-rectangle
-- min-pixel test), which correctly handles non-rectangular transparency
-- like wedge-shaped corners from alignment warping.
findOpaqueRect :: FilePath -> IO (Maybe (Int, Int, Int, Int))
findOpaqueRect maskFile = do
  mBbox <- getTrimBoundingBox maskFile
  case mBbox of
    Nothing -> do
      logWarnIO "findOpaqueRect: could not determine trim bounding box"
      return Nothing
    Just (trimW, trimH, trimX, trimY) -> do
      logInfoIO ("trim bounding box: " ++ showGeom trimW trimH trimX trimY)
      -- Outer bounds: never expand beyond the trim bounding box
      let outerTop = trimY
          outerBot = trimY + trimH - 1
          outerLeft = trimX
          outerRight = trimX + trimW - 1
      -- Quick check: is the trim box already fully opaque?
      alreadyOpaque <- isRectOpaque maskFile trimX trimY trimW trimH
      if alreadyOpaque
        then do
          logInfoIO "trim bounding box is already fully opaque"
          return (Just (trimX, trimY, trimW, trimH))
        else do
          -- Step 1: Find an initial opaque rect by uniform shaving.
          -- Binary search on the shave percentage (0-50%) from each edge.
          let maxShavePct = 50
          mInitShave <-
            bsearchFirst
              ( \pct -> do
                  let shaveX = (trimW * pct) `div` 200
                      shaveY = (trimH * pct) `div` 200
                      sx = trimX + shaveX
                      sy = trimY + shaveY
                      sw = trimW - 2 * shaveX
                      sh = trimH - 2 * shaveY
                  if sw <= 0 || sh <= 0
                    then return False
                    else isRectOpaque maskFile sx sy sw sh
              )
              0
              (maxShavePct + 1)
          case mInitShave of
            Nothing -> do
              logWarnIO "findOpaqueRect: no uniform shave percentage yields an opaque rectangle"
              return Nothing
            Just pct -> do
              let shaveX = (trimW * pct) `div` 200
                  shaveY = (trimH * pct) `div` 200
                  initLeft = trimX + shaveX
                  initTop = trimY + shaveY
                  initRight = trimX + trimW - 1 - shaveX
                  initBot = trimY + trimH - 1 - shaveY
              logInfoIO
                ( "initial opaque rect at "
                    ++ show pct
                    ++ "% shave: "
                    ++ showGeom (initRight - initLeft + 1) (initBot - initTop + 1) initLeft initTop
                )
              -- Step 2: Expand each edge outward toward trim bounds
              expand outerTop outerBot outerLeft outerRight initTop initBot initLeft initRight (0 :: Int)
  where
    maxIter = 20
    expand oTop oBot oLeft oRight topY botY leftX rightX iter
      | iter >= maxIter = do
          logInfoIO "findOpaqueRect: max expand iterations reached"
          returnRect topY botY leftX rightX
      | otherwise = do
          let w = rightX - leftX + 1
              h = botY - topY + 1
          logDebugIO
            ("expand iteration " ++ show iter ++ ": " ++ showGeom w h leftX topY)
          -- Expand top edge upward (find minimum topY in [oTop..topY])
          mNewTop <-
            bsearchFirst
              (\t -> isRectOpaque maskFile leftX t w (botY - t + 1))
              oTop
              (topY + 1)
          let newTopY = case mNewTop of Nothing -> topY; Just t -> t
          -- Expand bottom edge downward (find maximum botY in [botY..oBot])
          mNewBot <-
            bsearchLast
              (\b -> isRectOpaque maskFile leftX newTopY w (b - newTopY + 1))
              botY
              (oBot + 1)
          let newBotY = case mNewBot of Nothing -> botY; Just b -> b
          let newH2 = newBotY - newTopY + 1
          -- Expand left edge leftward (find minimum leftX in [oLeft..leftX])
          mNewLeft <-
            bsearchFirst
              (\l -> isRectOpaque maskFile l newTopY (rightX - l + 1) newH2)
              oLeft
              (leftX + 1)
          let newLeftX = case mNewLeft of Nothing -> leftX; Just l -> l
          -- Expand right edge rightward (find maximum rightX in [rightX..oRight])
          mNewRight <-
            bsearchLast
              (\r -> isRectOpaque maskFile newLeftX newTopY (r - newLeftX + 1) newH2)
              rightX
              (oRight + 1)
          let newRightX = case mNewRight of Nothing -> rightX; Just r -> r
          -- Check convergence
          if newTopY == topY && newBotY == botY && newLeftX == leftX && newRightX == rightX
            then do
              logInfoIO
                ( "findOpaqueRect converged: "
                    ++ showGeom (newRightX - newLeftX + 1) (newBotY - newTopY + 1) newLeftX newTopY
                )
              returnRect newTopY newBotY newLeftX newRightX
            else expand oTop oBot oLeft oRight newTopY newBotY newLeftX newRightX (iter + 1)
    returnRect topY botY leftX rightX =
      let w = rightX - leftX + 1
          h = botY - topY + 1
       in if w > 0 && h > 0
            then return (Just (leftX, topY, w, h))
            else return Nothing

cropToCommonIntersection :: Int -> FilePath -> Imgs -> IO Imgs
cropToCommonIntersection _ _ [] = return []
cropToCommonIntersection _ _ [img] = return [img]
cropToCommonIntersection fuzzPct wd imgs = do
  logInfoIO
    ( "finding fully-opaque crop rectangle for "
        ++ show (length imgs)
        ++ " images (fuzz="
        ++ show fuzzPct
        ++ "%)"
    )
  -- Create combined alpha mask: pixel-wise minimum of all alpha channels
  -- Threshold so that "almost opaque" (within fuzz%) counts as opaque.
  -- E.g. fuzz=10 means threshold at 90%: pixels with alpha >= 90% become white.
  let thresholdPct = show (100 - fuzzPct) ++ "%"
  withSystemTempDirectory "myphoto-crop" $ \tmpDir -> do
    let combinedMask = tmpDir </> "combined_alpha.png"
    logInfoIO "creating combined alpha mask (pixel-wise minimum across all images)..."
    let maskArgs =
          imgs
            ++ [ "-alpha",
                 "extract",
                 "-evaluate-sequence",
                 "Min",
                 "-threshold",
                 thresholdPct,
                 combinedMask
               ]
    (_, _, _, pHandle) <- createProcess (proc "magick" maskArgs)
    exitCode <- waitForProcess pHandle
    unless (exitCode == ExitSuccess) $
      fail ("creating combined alpha mask failed with " ++ show exitCode)
    logInfoIO "combined alpha mask created, searching for largest fully-opaque rectangle..."
    result <- findOpaqueRect combinedMask
    case result of
      Nothing -> do
        logWarnIO "no fully-opaque rectangle found, skipping crop"
        return imgs
      Just (cropX, cropY, cropW, cropH) -> do
        let geometryStr = showGeom cropW cropH cropX cropY
        logInfoIO ("fully-opaque rectangle: " ++ geometryStr)
        -- Check that cropping retains at least 85% of the original pixels
        (origW, origH) <- getImageSize (return (head imgs))
        let origPixels = origW * origH
            cropPixels = cropW * cropH
            ratio = fromIntegral cropPixels / fromIntegral origPixels :: Double
            minRatio = 0.85 :: Double
        if ratio < minRatio
          then do
            logWarnIO
              ( "crop would retain only "
                  ++ show (round (ratio * 100) :: Int)
                  ++ "% of pixels (minimum is "
                  ++ show (round (minRatio * 100) :: Int)
                  ++ "%), skipping crop"
              )
            return imgs
          else do
            logInfoIO
              ( "crop retains "
                  ++ show (round (ratio * 100) :: Int)
                  ++ "% of pixels, proceeding"
              )
            mapM
              ( \img -> do
                  let (bn, ext) = splitExtensions img
                      outImg = inWorkdir wd (takeFileName bn ++ "_CROPPED" ++ ext)
                  altOut <- findAltFileOfFile outImg
                  logInfoIO ("cropping " ++ img ++ " to " ++ geometryStr ++ " into " ++ altOut)
                  (_, _, _, pH) <-
                    createProcess
                      ( proc
                          "magick"
                          [img, "-crop", geometryStr, "+repage", altOut]
                      )
                  ec <- waitForProcess pH
                  unless (ec == ExitSuccess) $
                    fail ("cropping image failed with " ++ show ec)
                  return altOut
              )
              imgs
