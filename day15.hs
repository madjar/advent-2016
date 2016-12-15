#!/usr/bin/env stack
-- stack --install-ghc runghc --package trifecta

import Text.Trifecta
import Data.List
import Data.Maybe

data Disc = Disc Int Int Int deriving Show

parser :: Parser [Disc]
parser =
  many
    (Disc <$ symbol "Disc #" <*> int <* symbol "has" <*> int <*
     symbol "positions; at time=0, it is at position" <*>
     int <*
     symbol ".")
  where
    int = fromInteger <$> integer


main = do
  Just input <- parseFromFile parser "input15"
  print (solve input)


solve :: [Disc] -> Int
solve discs = fromJust (find solution [0..])
  where canPassDisc t (Disc n size pos) = (pos + n + t) `mod` size == 0
        solution t = all (canPassDisc t) discs
