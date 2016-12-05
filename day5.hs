#!/usr/bin/env stack
-- stack --install-ghc runghc --package cryptonite --package lens --package concurrent-output --package async
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Crypto.Hash
import qualified Data.ByteString.Char8 as C
import Data.Monoid
import System.IO
import Control.Lens
import Data.Char
import Data.List
import System.Console.Regions
import Control.Concurrent.Async

main = let hashes = interestingHashes "ugkcyxxp"
       in displayConsoleRegions $ firstPart hashes `concurrently` secondPart hashes

firstPart :: [String] -> IO ()
firstPart hashes =
  withConsoleRegion' Linear $
  \r -> mapM_ (appendConsoleRegion r . (\c ->[c])) (firstPassword hashes)

secondPart hashes =
  withConsoleRegion' Linear $
  \r -> mapM_ (setConsoleRegion r) (secondPassword hashes)

withConsoleRegion' layout a = withConsoleRegion layout $ \r -> a r <* (finishConsoleRegion r =<< getConsoleRegion r)

-- displayFirstStep = do
--   hSetBuffering stdout NoBuffering
--   putStrLn (compute "ugkcyxxp")

-- compute = firstPassword . interestingHashes
-- compute' = secondPassword . interestingHashes

firstPassword = take 8 . map (!! 5)

secondPassword = takeUntil fullPassword . scanl updatePassword "________"
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
