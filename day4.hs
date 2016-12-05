#!/usr/bin/env stack
-- stack --install-ghc runghc --package trifecta

import Text.Trifecta
import Control.Applicative
import Data.Map (fromListWith, toList)
import Data.List
import Data.Tuple
import Data.Maybe
import Data.Ord
import Data.Monoid
import Control.Arrow

data Room = Room String Integer String deriving Show

sector (Room _ s _) = s

parser = parseRoom `endBy1` newline

parseRoom = Room <$> encrypted <*> sector <*> checksum
  where encrypted = init <$> some (lower <|> char '-')
        sector = integer
        checksum = between (char '[') (char ']') (some lower)

main = mapM_ print . map (sector &&& decrypt) . filter isReal . fromJust =<< parseFromFile parser "input4"

isReal (Room enc _ checksum) = expectedChecksum == checksum
  where expectedChecksum = take 5 . map fst . checkSumSort . frequency . filter (/= '-') $ enc
        checkSumSort = sortBy (comparing (Down . snd) <> comparing fst)

frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = toList (fromListWith (+) [(x, 1) | x <- xs])

decrypt (Room enc sector _) = map (nTimes n shift) enc
  where n = sector `mod` 26


shift '-' = ' '
shift ' ' = ' '
shift 'z' = 'a'
shift l = succ l

nTimes :: Integer -> (a -> a) -> (a -> a)
nTimes 0 _ = id
nTimes 1 f = f
nTimes n f = f . nTimes (n-1) f
