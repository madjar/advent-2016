{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
#!/usr/bin/env stack
-- stack --install-ghc runghc

import Debug.Trace
import qualified Data.Sequence as S

input :: Int
input = 3001330

main = print (solve2 input)

solve :: Int -> Int
solve n = solve' (map (, 1) [1..n])

solve' :: [(Int, Int)] -> Int
solve' [(elf, _)] = elf
solve' xs = traceShow (length xs) $ solve' (go xs [])
  where go ((elf, n1):(_, n2):rest) accu = go rest ((elf, n1 + n2):accu)
        go xs accu = xs ++ (reverse accu)

solve2 :: Int -> Int
solve2 n = solve2' $ S.fromList (map (, 1) [1..n])

solve2' :: S.Seq (Int, Int) -> Int
solve2' (S.viewl -> (elf, n) S.:< rest) = traceShow size $ if size == 1
  then elf
  else let !indexToSteal = (size `div` 2) - 1
           (_, !stolen) = S.index rest indexToSteal
           !newRest = deleteAt indexToSteal rest
       in solve2' $ newRest S.|> (elf, n + stolen)
  where !size = S.length (rest) + 1


deleteAt :: Int -> S.Seq a -> S.Seq a
deleteAt i s = S.take i s S.>< S.drop (i + 1) s
