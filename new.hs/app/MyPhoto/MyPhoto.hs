module MyPhoto.MyPhoto
  ( runMyPhoto
  ) where

import           Control.Monad              (unless, when)
import qualified System.IO                  as IO

import MyPhoto.Model

import MyPhoto.Actions.FocusStack 
import MyPhoto.Actions.Enfuse

runMyPhoto :: IO ()
runMyPhoto = do
    (opts@Options{optVerbose = verbose}, imgs) <- getOptions
    setCurrentWD opts
    when verbose $ print opts

    (focusStacked,aligned) <- focusStackImgs opts imgs
    
    enfuseStacked <- enfuseStackImgs (enfuseDefaultOptions{optOutputBN = Just (computeStackOutputBN imgs)}) aligned

    -- shakeArgs shakeOptions{shakeFiles="_myphoto", shakeVerbosity=Diagnostic} $ do
    --   focusStack <- focusStackRules opts imgs

    --   want [focusStack]

    IO.hPutStrLn IO.stderr ("Done")
