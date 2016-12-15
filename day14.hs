#!/usr/bin/env stack
-- stack --install-ghc runghc --package cryptonite --package parallel
{-# LANGUAGE OverloadedStrings #-}

import Crypto.Hash
import qualified Data.ByteString.Char8 as C
import Data.Monoid
import Data.List
import Control.Arrow
import Data.Maybe
import Control.Parallel.Strategies

salt = "ahsbgdzn"
--salt = "abc"

main = print (keys !! 63)

keys = map head . filter isKey . tails $ potentialKeys
  where
    isKey ((index, _, char):rest) =
      any (\(_, count, char') -> char == char' && count >= 5) .
      takeWhile (\(i, _, _) -> i <= index + 1000) $ rest

potentialKeys :: [(Int, Int, Char)]
potentialKeys =
  catMaybes $
  withStrategy (parBuffer 100 rdeepseq) $
  zipWith flatten [0 ..] (map analyse hashes)
  where
    flatten index (Just (count, char)) = Just (index, count, char)
    flatten _ Nothing = Nothing

hashes :: [String]
hashes = (map (nTimes 2016 (md5 . C.pack) . md5 . makeStrToHash) $ [0 ..])
  where
    makeStrToHash = (salt <>) . C.pack . show
    md5 = show . (hash :: C.ByteString -> Digest MD5)

analyse :: String -> Maybe (Int, Char)
analyse = find (\(l, _) -> l >= 3) . map (length &&& head) . group


nTimes :: Int -> (a -> a) -> (a -> a)
nTimes 0 _ = id
nTimes 1 f = f
nTimes n f = f . nTimes (n-1) f
