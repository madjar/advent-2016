#!/usr/bin/env stack
-- stack --install-ghc runghc --package trifecta

import Text.Trifecta
import Data.Maybe
import Control.Applicative
--import Data.Array.MArray
import Data.Ix
import Debug.Trace
import Control.Monad

data Inst
  = Rect Integer
         Integer
  | RotateRow Integer
              Integer
  | RotateColumn Integer
                 Integer
  deriving (Show)

main = do
  Just input <- parseFromFile parser "input8"
  let display = solve input
  printDisplay display
  print . length . filter display $ displayRange

parser = some instruction
  where
    instruction = rect <|> (symbol "rotate" *> (col <|> row))
    rect = Rect <$ symbol "rect" <*> natural <* char 'x' <*> natural
    col =
      RotateColumn <$ symbol "column x=" <*> natural <* symbol "by" <*> natural
    row = RotateRow <$ symbol "row y=" <*> natural <* symbol "by" <*> natural

dimX = 50
dimY = 6

displayRange = range ((0, 0), (dimX - 1, dimY - 1))

printDisplay display =
  forM_ [0 .. dimY - 1] $
  \y -> do
    forM_ [0 .. dimX - 1] $
      \x ->
         putStr
           (if display (x, y)
              then "#"
              else ".")
    putStrLn ""

emptyDisplay _ = False

update display (Rect rx ry) (x, y) =
  if x < rx && y < ry
    then True
    else display (x, y)
update display (RotateRow ry by) (x, y) =
  if y == ry
    then display ((x + dimX - by) `mod` dimX, y)
    else display (x, y)
update display (RotateColumn rx by) (x, y) =
  if x == rx
    then display (x, (y + dimY - by) `mod` dimY)
    else display (x, y)

solve :: [Inst] -> (Integer, Integer) -> Bool
solve = foldl update emptyDisplay
