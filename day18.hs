#!/usr/bin/env stack
-- stack --install-ghc runghc


input = "^.^^^.^..^....^^....^^^^.^^.^...^^.^.^^.^^.^^..^.^...^.^..^.^^.^..^.....^^^.^.^^^..^^...^^^...^...^."

solve = length . filter (== '.') . concat . take 400000 . iterate nextRow $ input

nextRow :: String -> String
nextRow row = map rules $ window 3 ("." ++ row ++ ".")

rules :: String -> Char
rules previous = case previous of
  "^^." -> '^'
  ".^^" -> '^'
  "^.." -> '^'
  "..^" -> '^'
  _ -> '.'

window :: Int -> [a] -> [[a]]
window n xs =
  if (length xs) < n
    then []
    else (take n xs) : window n (tail xs)
