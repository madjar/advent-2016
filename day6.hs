#!/usr/bin/env stack
-- stack --install-ghc runghc

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List
import Data.Ord


main = print . solve . lines =<< readFile "input6"


solve :: [String] -> String
solve = map bestChar . foldl merge empty
  where empty = replicate 8 Map.empty
        merge = zipWith (flip addCounter)
        bestChar = fst . minimumBy (comparing snd) . Map.assocs

type Counter = Map Char Int

addCounter :: Char -> Counter -> Counter
addCounter char = Map.insertWith (+) char 1
