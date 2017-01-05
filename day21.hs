#!/usr/bin/env stack
-- stack --install-ghc runghc --package trifecta

import Text.Trifecta
import qualified Data.Sequence as Seq
import Control.Applicative
import Data.Maybe
import Debug.Trace
import Test.QuickCheck

startSeq = Seq.fromList "abcdefgh"
secondSeq = Seq.fromList "fbgdceah"

main = do
  Just input <- parseFromFile (some parser <* eof) "input21"
  let transfo = foldl1 (.) input
  print (transfo secondSeq)

parser :: Parser (Seq.Seq Char -> Seq.Seq Char)
parser = (swapP' <$ symbol "swap position" <*> int <* symbol "with position" <*> int)
  <|> (swapL' <$ symbol "swap letter" <*> ch <* symbol "with letter" <*> ch)
  <|> (rotateL' <$ symbol "rotate left" <*> int <* (symbol "steps" <|> symbol "step"))
  <|> (rotateR' <$ symbol "rotate right" <*> int <* (symbol "steps" <|> symbol "step"))
  <|> (rotateP' <$ symbol "rotate based on position of letter" <*> ch)
  <|> (reverseP' <$ symbol "reverse positions" <*> int <* symbol "through" <*> int)
  <|> (moveP' <$ symbol "move position" <*> int <* symbol "to position" <*> int)
  where int = fromInteger <$> integer
        ch = token anyChar

-- Part 1

swapP x y s = Seq.update x (Seq.index s y) $ Seq.update y (Seq.index s x) $ s

swapL x y s =
  Seq.update (fromJust $ Seq.elemIndexL x s) y $
  Seq.update (fromJust $ Seq.elemIndexL y s) x $ s

rotateR x s = rotateL (Seq.length s - x) s

rotateL y s = Seq.mapWithIndex get s
  where get pos _ = Seq.index s ((pos + y) `mod` Seq.length s)

rotateP x s = rotateR count s
  where pos = fromJust $ Seq.elemIndexL x s
        count = if pos >= 4
                  then pos + 2
                  else pos + 1

reverseP x y s = pre Seq.>< Seq.reverse toReverse Seq.>< post
  where (pre, rest) = Seq.splitAt x s
        (toReverse, post) = Seq.splitAt (y - x + 1) rest

moveP x y s = insertAt y letter $ deleteAt x $ s
  where letter = Seq.index s x

-- Part 2
swapP' = swapP
swapL' = swapL
rotateR' = rotateL
rotateL' = rotateR

rotateP' x s = rotateL count s
  where
    pos = fromJust $ Seq.elemIndexL x s
    count =
      if pos == 0
        then 1
        else if odd pos
               then (pos + 1) `div` 2
               else (pos `div` 2) + 5

reverseP' = reverseP
moveP' x y = moveP y x


-- Utils that should be in Data.Sequence

deleteAt :: Int -> Seq.Seq a -> Seq.Seq a
deleteAt i s = Seq.take i s Seq.>< Seq.drop (i + 1) s

insertAt :: Int -> a -> Seq.Seq a -> Seq.Seq a
insertAt i x s = (Seq.take i s Seq.|> x) Seq.>< Seq.drop i s

-- Tests

genInput = Seq.fromList <$> shuffle "abcdefgh"
genChar = choose ('a', 'h')
genIndex = choose (0, 7)
gen = (,,,,) <$> genInput <*> genChar <*> genChar <*> genIndex <*> genIndex


test = quickCheck (forAll gen (\(s, a, b, x, y) ->
                                 swapP' x y (swapP x y s) === s
                                 .&&. swapL' a b (swapL a b s) === s
                                 .&&. rotateR' x (rotateR x s) === s
                                 .&&. rotateL' x (rotateL x s) === s
                                 .&&. rotateP' a (rotateP a s) === s
                                 -- .&&. reverseP' x y (reverseP x y s) === s
                                 -- .&&. moveP' x y (moveP y x s) === s
                                 ))
