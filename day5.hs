#!/usr/bin/env stack
-- stack --install-ghc runghc --package cryptonite --package lens
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Crypto.Hash
import qualified Data.ByteString.Char8 as C
import Data.Monoid
import System.IO
import Control.Lens
import Data.Char
import Data.List

main = mapM_ putStrLn (compute' "ugkcyxxp")

displayFirstStep = do
  hSetBuffering stdout NoBuffering
  putStrLn (compute "ugkcyxxp")


compute = take 8 . map (!! 5) . interestingHashes

compute' = takeUntil fullPassword . scanl updatePassword "________" . interestingHashes
  where takeUntil p = foldr (\x ys -> x : if p x then [] else ys) []
        fullPassword = not . ('_' `elem`)

updatePassword :: String -> String -> String
updatePassword password hash = if currentChar == Just '_'
                                 then password & ix pos .~ char
                                 else password
  where (rawPos:char:_) = drop 5 hash
        pos = digitToInt rawPos
        currentChar = password ^? ix pos

interestingHashes :: C.ByteString -> [String]
interestingHashes input = filter indicatesNextChar . map (md5 . makeStrToHash input) $ [0 ..]
  where makeStrToHash input = (input <>) . C.pack . show
        md5 = show . (hash :: C.ByteString -> Digest MD5)
        indicatesNextChar hash = take 5 hash == "00000"
