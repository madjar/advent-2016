#!/usr/bin/env stack
-- stack --install-ghc runghc --package extra

import Data.List.Extra

input = "10010000000110000"
toFill = 272
toFill' = 35651584

main = print solution

solution = checksum (take toFill' (dragon input))

dragon :: String -> String
dragon x = concat $ x : unfoldr mkNext x
  where mkNext s = let next = "0" ++ secondPart s in Just (next, s ++ next)
        secondPart = reverse . notS

notS = map notC
  where notC '0' = '1'
        notC '1' = '0'


checksum :: String -> String
checksum s
  | odd (length s) = s
  | otherwise = checksum . map reduce . chunksOf 2 $ s
  where
    reduce [a, b] =
      if a == b
        then '1'
        else '0'
