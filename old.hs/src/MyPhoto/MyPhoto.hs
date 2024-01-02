{-# LANGUAGE LambdaCase #-}

module MyPhoto.MyPhoto
  ( module X,
    runMyPhoto,
    actions,
    composeActions,
  )
where

import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import MyPhoto.Actions.Align as X
import MyPhoto.Actions.Copy as X
import MyPhoto.Actions.Crop as X
import MyPhoto.Actions.Montage as X
import MyPhoto.Actions.Outliers as X
import MyPhoto.Actions.Pwd as X
import MyPhoto.Actions.Show as X
import MyPhoto.Actions.Skip as X
import MyPhoto.Actions.Stack as X
import MyPhoto.Actions.Thinning as X
import MyPhoto.Actions.ToJPG as X
import MyPhoto.Actions.UnRAW as X
import MyPhoto.Actions.UnTiff as X
import MyPhoto.Actions.Wait as X
import MyPhoto.Model as X
import MyPhoto.Utils as X
import System.Environment
import System.Exit

actions :: Map String PrePAction
actions =
  Map.fromList
    [ ("unraw", unRAW),
      ("untiff", unTiff),
      ("tojpg", toJPG),
      ("crop", crop),
      ("copy", copyPAct),
      ("link", linkPAct),
      ("myphotoout", myphotooutPAct),
      ("sparse", sparsePAct),
      ("thinning", thinningPAct),
      ("breaking", breakingPAct),
      ("rmoutliers", rmOutliers),
      ("align", align),
      ("stack", stack),
      ("wait", waitPAct),
      ("show", showImgs),
      ("pwd", pwdPAct),
      ("skip", skipPAct),
      ("montage", montage)
    ]

type ComposeActionsState = (PAction, [String], Maybe PrePAction)

composeActions :: [String] -> (PAction, [Img])
composeActions =
  let composeActions' :: ComposeActionsState -> String -> ComposeActionsState
      composeActions' (act, opts, Nothing) opt = case opt `Map.lookup` actions of
        Just preAct -> (act, [], Just preAct) -- TODO: assert that otps were empty
        Nothing -> (act, opts ++ [opt], Nothing)
      composeActions' (act, opts, Just preAct) "--" = (act <> endLine <> preAct opts, [], Nothing)
      composeActions' (act, opts, Just preAct) opt = case opt `Map.lookup` actions of
        Just preAct2 -> (act <> endLine <> preAct opts, [], Just preAct2)
        Nothing -> (act, opts ++ [opt], Just preAct)
   in (\(act, imgs) -> (act <> endLine <> logSeparator "Result:", imgs))
        . ( \case
              (act, imgs, Nothing) -> (act, imgs)
              (act, opts, Just preAct) -> (act <> endLine <> preAct opts, [])
          )
        . foldl composeActions' (mempty, [], Nothing)

help :: IO ()
help = do
  putStrLn "myphoto action [actArg [actArg ..]] [action [actArg [actArg ..]] ..] -- [img [img ...]]"
  putStrLn "myphoto autostackmv -- [img [img ...]]"
  putStrLn "myphoto autostack -- [img [img ...]]"
  putStrLn "myphoto autostackraw -- [img [img ...]]"
  putStrLn ""
  mapM_
    ( \(k, preAct) -> do
        putStrLn ""
        putStrLn line
        putStrLn k
        runPAction (preAct ["-h"]) [] >>= \case
          Left err -> putStrLn err
          _ -> mempty
    )
    (Map.assocs actions)

applyHigherOrderArgs :: [String] -> [String]
applyHigherOrderArgs [] = []
applyHigherOrderArgs args@("--" : _) = args
applyHigherOrderArgs args = case args of
  -- ("autostackmv":oArgs) -> applyHigherOrderArgs $ "autostack":"myphotoout":oArgs
  ("autostack" : oArgs) ->
    -- [ "skip", "1"
    -- , "breaking", "20"
    [ "rmoutliers",
      "thinning",
      "1",
      "align",
      "-f",
      "untiff",
      "--rm",
      "stack",
      "--autochunk",
      "--threads",
      "myphotoout"
    ]
      ++ oArgs
  ("autostackraw" : oArgs) ->
    -- [ "skip", "1"
    -- , "breaking", "20"
    [ "thinning",
      "1",
      "unraw",
      "--wb1",
      "rmoutliers",
      "untiff",
      "--rm",
      "align",
      "-f",
      "untiff",
      "--rm",
      "stack",
      "--autochunk",
      "--threads",
      "myphotoout"
    ]
      ++ oArgs
  arg : oArgs -> arg : applyHigherOrderArgs oArgs

printArgs :: [String] -> IO ()
printArgs args = putStrLn (unwords (takeWhile (/= "--") args) ++ " -- [img [img ...]]")

runMyPhoto :: IO ()
runMyPhoto = do
  args <- getArgs

  when (args == ["-h"]) $ do
    help
    exitSuccess

  let args' = applyHigherOrderArgs args

  printArgs args'

  let (act, imgs) = composeActions args'
  result <- runPAction act imgs
  case result of
    Left err -> putStrLn err
    Right imgs' -> mapM_ putStrLn imgs'
