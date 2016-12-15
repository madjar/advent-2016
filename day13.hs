#!/usr/bin/env stack
-- stack --install-ghc runghc --package astar

import           Control.Monad
import           Data.Bits
import           Data.Graph.AStar
import qualified Data.HashSet     as Set
import           Data.Ix
import           Data.Maybe

input = 1352

type Maze = (Int, Int) -> Bool

main = do
  printMaze 40 maze solution
  print (length solution)
  -- Part 2 is going to be ugly
  print . length . filter (\p -> isReacheableIn (1, 1) p 50 maze) $
    range ((0, 0), (50, 50))

solution = fromJust $ solve (1, 1) (31, 39) maze

printMaze :: Int -> Maze -> [(Int, Int)] -> IO ()
printMaze size maze sol = do
  forM_ [0..size] $ \y -> do
    forM_ [0..size] $ \x -> do
      putStr (display (x, y))
    putStrLn ""
  where display p | p `elem` sol = "O"
                  | maze p = "."
                  | otherwise = "#"

maze :: Maze
maze (x, y) = x >= 0 && y >= 0 && popCount magic `mod` 2 == 0
  where magic = x*x + 3*x + 2*x*y + y + y*y + input

solve :: (Int, Int) -> (Int, Int) -> Maze -> Maybe [(Int, Int)]
solve begin end@(ex, ey) maze = aStar neighbours distance heur goal begin
  where neighbours (x, y) = Set.fromList (filter maze [(x-1,y),(x+1,y),(x,y-1),(x,y+1)])
        distance _ _ = 1
        heur (x, y) = abs (x - ex) + abs (y - ey)
        goal = (== end)

isReacheableIn :: (Int, Int) -> (Int, Int) -> Int -> Maze -> Bool
isReacheableIn begin end distance maze =
  case solve begin end maze of
    Just sol -> length sol <= distance
    Nothing -> False
