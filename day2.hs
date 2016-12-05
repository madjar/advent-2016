#!/usr/bin/env stack
-- stack --install-ghc runghc --package array --trifecta

import Text.Trifecta
import Control.Applicative
import Control.Monad
import Data.Array
import Data.Maybe

data Inst = U | D | L | R deriving (Show)

main = print =<< solve . fromJust <$> parseFromFile parser "input2"

parser :: Parser [[Inst]]
parser = some inst `endBy1` newline
  where inst = pairParser ['U' --> U, 'D' --> D, 'L' --> L,  'R' --> R]
        pairParser :: [(Char, Inst)] -> Parser Inst
        pairParser = msum . map (\(c, v) -> char c >> pure v)
        (-->) = (,)

type Pos = (Int, Int)

keyrange = ((1, 1), (5, 5))
keypad = listArray keyrange ("  1  \
                             \ 234 \
                             \56789\
                             \ ABC \
                             \  D  ")
printKey k = keypad ! k

solve :: [[Inst]] -> String
solve = tail . map printKey . scanl nextPos (3, 1)

nextPos :: Pos -> [Inst] -> Pos
nextPos = foldl move

move :: Pos -> Inst -> Pos
move (x, y) inst = if keyrange `inRange` newPos && (keypad ! newPos /= ' ')
                     then newPos
                     else (x, y)
  where newPos = case inst of
          U -> (x-1, y)
          D -> (x+1, y)
          L -> (x, y-1)
          R -> (x, y+1)
