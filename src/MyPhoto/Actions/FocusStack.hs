module MyPhoto.Actions.FocusStack
  ( focusStackImgs,
  )
where

import qualified Data.Map as Map
import MyPhoto.Model
import qualified System.IO as IO
import System.Process
import MyPhoto.Wrapper.FocusStack

focusStackImgs :: Bool -> [String] -> [FilePath] -> IO (FilePath, [FilePath])
focusStackImgs verbose additionalParameters imgs = do
  output <- makeAbsolute (computeStackOutputBN imgs ++ "_focus-stack.png")
  let focusStackWorkdir = output -<.> "workdir"

  outputExists <- doesFileExist output
  if outputExists
    then do
      IO.hPutStrLn IO.stderr $ "INFO: focus-stack output " ++ output ++ " already exists"
      let alignedImgs = computeAlignedImgs focusStackWorkdir imgs
      return (output, alignedImgs)
    else do
      runFocusStack
        FocusStackOptions
          { _verbose = verbose,
            _cropping = FocusStackNoCrop,
            _additionalParameters = additionalParameters,
            _imgs = imgs,
            _workdir = focusStackWorkdir,
            _output = output
          }
      


--       createDirectoryIfMissing True focusStackWorkdir
--       putStrLn (unwords ["$ focus-stack", unwords parameters, "[img [img [...]]]"])
--       (_, _, _, ph) <-
--         createProcess
--           ( proc
--               "focus-stack"
--               (parameters ++ imgs)
--           )
--             { cwd = Just focusStackWorkdir
--             }
--       exitcode <- waitForProcess ph
--       if exitcode /= ExitSuccess
--         then do
--           fail $ "ERR: focus-stack exited with " ++ (show exitcode)
--         else do
--           return ()
--       exists <- doesFileExist outputName
--       unless exists $ do
--         fail $ "image not found: " ++ outputName
--   mapM_
--     ( \img -> do
--         exists <- doesFileExist img
--         unless exists $ do
--           fail $ "image not found: " ++ img
--     )
--     alignedImgs

--   return (outputName, alignedImgs)
