module MyPhoto.Utils.ProgressBar
  ( Progress (..),
    ProgressBar,
    incProgress,
    newProgressBarDefault,
    newImgsProgressBar,
  )
where

import System.ProgressBar (Progress (..), ProgressBar, ProgressBarWidth (ConstantWidth), Style (..), defStyle, incProgress, newProgressBar)

-- | Shared progress bar style capped to 130 characters to avoid overly wide output.
progressBarStyle :: Style s
progressBarStyle = defStyle {styleWidth = ConstantWidth 100}

newProgressBarDefault :: Progress s -> IO (ProgressBar s)
newProgressBarDefault = newProgressBar progressBarStyle 10

newImgsProgressBar :: [a] -> IO (ProgressBar ())
newImgsProgressBar imgs = newProgressBarDefault (Progress 0 (length imgs) ())
