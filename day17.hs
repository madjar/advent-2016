#!/usr/bin/env stack
-- stack --install-ghc runghc --package astar
{-# LANGUAGE OverloadedStrings #-}

import Crypto.Hash
import qualified Data.ByteString.Char8 as C
import Data.Monoid
import Debug.Trace
import Data.Graph.AStar
import qualified Data.HashSet as S

input = "vkjiggvb"


solve = aStar (S.fromList . neighbours) distance heur goal start
  where distance _ _ = 1
        heur path = let (x, y) = position path in 3 - x + 3 - y
        goal path = position path == (3, 3)
        start = ""

longestPath :: Path -> Int
longestPath path
--  | traceShow path False = undefined
  | position path == (3, 3) = length path
  | otherwise = maximum . (<> [0]) . map longestPath . neighbours $ path

type Path = String
type Dir = Char
md5 = show . (hash :: C.ByteString -> Digest MD5) . C.pack

open :: Path -> Dir -> Bool
open path =
  \dir ->
     case dir of
       'U' -> isOpen u
       'D' -> isOpen d
       'L' -> isOpen l
       'R' -> isOpen r
  where
    u:d:l:r:_ = md5 (input <> path)
    isOpen = (>= 'b')

position :: Path -> (Int, Int)
position = foldl move (0, 0)

move :: (Int, Int) -> Dir -> (Int, Int)
move (x, y) = \dir -> case dir of
  'U' -> (x, y - 1)
  'D' -> (x, y + 1)
  'L' -> (x - 1, y)
  'R' -> (x + 1, y)

neighbours :: Path -> [Path]
neighbours path = filter exists . map (\d -> path <> [d]) . filter isOpen $ "UDLR"
  where isOpen dir = open path dir
        exists p = let (x, y) = position p in x >= 0 && x < 4 && y >= 0 && y < 4


