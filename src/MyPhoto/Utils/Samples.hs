module MyPhoto.Utils.Samples
  ( everyNth,
    sampleOfM
  )
where

everyNth :: Int -> [a] -> [a]
everyNth _ [] = []
everyNth n (x : xs) =
  let everyNth' :: Int -> [a] -> [a]
      everyNth' n xs = case drop (n - 1) xs of
        (y : ys) -> y : everyNth n ys
        [] -> [last xs | not (null xs)]
   in x : everyNth' n xs
  
sampleOfM :: Int -> [a] -> [a]
sampleOfM _ [] = []
sampleOfM m xs =
  let len = length xs
      n = len `div` m
   in if len <= m
        then xs
        else everyNth n xs

