#!/usr/bin/env stack
-- stack --install-ghc runghc --package trifecta

import Text.Trifecta
import Control.Applicative
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)

main = do
  Just input <- parseFromFile parser "input22"
  print $ length $ viablePairs input

type Node = (Int, Int)
data Usage = Usage { used :: Int, avail :: Int } deriving Show

parser :: Parser ([(Node, Usage)])
parser = do
  many (notChar '\n')
  newline
  many (notChar '\n')
  newline
  many line
  where line = (,) <$> node <*> usage
        node = (,) <$ symbol "/dev/grid/node-x" <*> int <* symbol "-y" <*> int
        usage = Usage <$ size <*> size <*> size <* int <* symbol "%"
        size = int <* symbol "T"
        int = fromInteger <$> integer

viablePairs :: [(Node, Usage)] -> [((Node, Usage), (Node, Usage))]
viablePairs nodes = [(a, b) | a <- nodes, b <- nodes, viable a b]
  where
    viable (na, Usage ua _) (nb, Usage _ ab) = na /= nb && ua /= 0 && ua <= ab
