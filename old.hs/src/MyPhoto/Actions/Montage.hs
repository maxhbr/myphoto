module MyPhoto.Actions.Montage
  ( montage,
  )
where

import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.MSem as MS
import Control.Monad
import qualified Data.ByteString as B
import Data.Maybe (maybe)
import qualified Data.Vector as V (fromList)
import GHC.Conc (numCapabilities)
import Graphics.Netpbm
import MyPhoto.Model
import MyPhoto.Utils
import qualified Statistics.Sample as S
import System.Console.GetOpt
import System.Directory
import System.Exit
import System.FilePath
import System.IO.Temp
import System.Process

help :: String
help =
  let header = "Usage: montage OUTPUT_FILE"
   in usageInfo header options

data Options = Options
  { optOutputFile :: Maybe FilePath,
    optHelp :: Bool
  }
  deriving (Show)

defaultOptions :: Options
defaultOptions =
  Options
    { optOutputFile = Nothing,
      optHelp = False
    }

options :: [OptDescr (Options -> Options)]
options =
  [ Option
      ['o']
      ["--output-file"]
      ( OptArg
          (\fp opts -> opts {optOutputFile = fp})
          "OUTPUT_FILE"
      )
      "file to wriite the montage to (expected to be e.g. a PNG)",
    Option
      ['h']
      ["help"]
      (NoArg (\opts -> opts {optHelp = True}))
      "print help"
  ]

getMyOpts :: [String] -> IO (Options, [String])
getMyOpts argv = case getOpt Permute options argv of
  (o, n, []) -> return (foldl (flip id) defaultOptions o, n)
  (_, _, errs) -> ioError (userError (concat errs ++ help))

montageImpl :: [String] -> [Img] -> PActionBody
montageImpl _ [] = return (Right [])
montageImpl args imgs@(img1 : _) = do
  (opts, _) <- getMyOpts args

  if optHelp opts
    then return (Left help)
    else
      let (bn, ext) = splitExtensions img1
          outputFile = case optOutputFile opts of
            Just outputFile' -> outputFile'
            Nothing -> bn ++ "_" ++ show (length imgs) ++ "_MONTAGE.png"
       in do
            (_, _, _, pHandle) <- createProcess (proc "montage" (["-geometry", "100x100+2+2"] ++ imgs ++ [outputFile]))
            exitCode <- waitForProcess pHandle
            unless (exitCode == ExitSuccess) $
              fail ("Resize failed with " ++ show exitCode)
            return (Right imgs)

montage :: PrePAction
montage args = logSeparator "montage" <> PAction (montageImpl args)
