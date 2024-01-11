{-# LANGUAGE LambdaCase #-}

module MyPhoto.Wrapper.DcrawWrapper
  ( runDcraw,
    getImageSize,
    calculateWhitebalance,
  )
where

import Data.List (find, isPrefixOf)
import GHC.IO.Handle (hGetContents)
import System.Exit
import System.Process

runDcraw :: [String] -> IO (ExitCode, String, String)
runDcraw args = do
  (_, Just hout, Just herr, pHandle) <- createProcess (proc "dcraw_emu" args) {std_out = CreatePipe, std_err = CreatePipe}
  exitCode <- waitForProcess pHandle
  out <- hGetContents hout
  err <- hGetContents herr
  return (exitCode, out, err)

getImageSize :: FilePath -> IO (Int, Int)
getImageSize img =
  let imageSizePrefix = "Image size:  "
   in do
        putStrLn ("calculate image size from " ++ img)
        (exitCode, out, err) <- runDcraw ["-v", "-i", img]
        case exitCode of
          ExitSuccess -> case find (imageSizePrefix `isPrefixOf`) (lines out) of
            Just l ->
              let str = drop (length imageSizePrefix) l
                  strItems = words str
                  x = read (strItems !! 0) :: Int
                  y = read (strItems !! 2) :: Int
               in do
                    putStrLn ("Image size: " ++ show x ++ "x" ++ show y)
                    return (x, y)
            Nothing -> fail ("Failed to get image size in : " ++ show out)
          _ -> fail ("Failed to get image size of " ++ img ++ " with " ++ show exitCode)

calculateWhitebalance :: FilePath -> IO [String]
calculateWhitebalance wbImg =
  let multipliersPrefix = "multipliers "
      getDcrawWBArgs :: (Int, Int) -> [String]
      getDcrawWBArgs (xSize, ySize) =
        let xMid = xSize `div` 2
            yMid = ySize `div` 2
            xHeight = xSize `div` 3
            yHeight = ySize `div` 3
         in ["-v", "-A", show xMid, show yMid, show xHeight, show yHeight, "-c"]
   in do
        size <- getImageSize wbImg
        putStrLn ("calculate WB from " ++ wbImg)
        let args = getDcrawWBArgs size ++ [wbImg]
        -- (_, _, Just herr, pHandle) <- createProcess (proc "dcraw_emu" args) {std_out = NoStream, std_err = CreatePipe}
        -- exitCode <- waitForProcess pHandle
        -- err <- hGetContents herr
        (exitCode, _, err) <- runDcraw args
        case exitCode of
          ExitSuccess -> case find (multipliersPrefix `isPrefixOf`) (lines err) of
            Just l ->
              let multipliers = words (drop (length multipliersPrefix) l)
               in do
                    putStrLn ("multipliers: " ++ show multipliers)
                    return multipliers
            Nothing -> fail ("Failed to find multipliers in " ++ show err)
          _ -> fail ("Failed to calculate WB of " ++ wbImg ++ " with " ++ show exitCode)
