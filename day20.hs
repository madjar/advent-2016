#!/usr/bin/env stack
-- stack --install-ghc runghc --package trifecta

import Text.Trifecta
import Data.List
import Data.Ix

parser = many range
  where range = (,) <$> int <* symbolic '-' <*> int
        int = fromInteger <$> integer


main = do
  Just input <- parseFromFile parser "input20"
  let merged = mergeRanges input
  print (snd (head merged) + 1)
  print (allowed merged)


mergeRanges :: [(Int, Int)] -> [(Int, Int)]
mergeRanges = merge . sort
  where
    merge ((a, b):(c, d):rest) =
      if (b + 1) >= c
        then merge ((a, max b d) : rest)
        else (a, b) : merge ((c, d) : rest)
    merge xs = xs

allowed :: [(Int, Int)] -> Int
allowed ranges = 4294967296 - sum (map count ranges)
  where count (a, b) = b - a + 1
