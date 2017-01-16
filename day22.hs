{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
#!/usr/bin/env stack
-- stack --install-ghc runghc --package trifecta

import Text.Trifecta
import Control.Applicative
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import qualified Data.HashSet as Set
import Data.HashSet (HashSet)
import Data.Graph.AStar
import Data.Hashable (Hashable)
import GHC.Generics
import Data.Maybe
import Debug.Trace

main = do
  Just input <- parseFromFile parser "input22"
  --print $ length $ viablePairs input
  let Just solution = solve input
--      display (nodes, goal) = print (lookup goal nodes, goal)
--  mapM_ display solution
  print (length solution)


-- Part 1

type Node = (Int, Int)
data Usage = Usage { used :: Int, avail :: Int } deriving (Show, Ord, Eq, Hashable, Generic)

total (Usage u a) = a + u
emptied (Usage u a) = Usage 0 (u + a)
moved x (Usage u a) = Usage (u + x) (a - x)

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

viable (na, Usage ua _) (nb, Usage _ ab) = na /= nb && ua /= 0 && ua <= ab

-- Part 2

--type Situation = ([(Node, Usage)], Node)

data Situation = Situation
  { obstacles :: [Node]
  , emptyN :: Node
  , goal :: Node
  } deriving (Show, Ord, Eq, Hashable, Generic)

simplify :: [(Node, Usage)] -> Situation
simplify nodes = Situation {..}
  where obstacles = map fst $ filter ((>100) . used . snd) nodes
        [emptyN] = map fst $ filter ((== 0) . used . snd) nodes
        goal = (38, 0)

solve :: [(Node, Usage)] -> Maybe [Situation]
solve = aStar neighbours distance heuristic isGoal . simplify
  where distance _ _ = 1

neighbours :: Situation -> HashSet Situation
neighbours s = Set.fromList . map doMove . filter (not . obs) . filter inRange $ plane (emptyN s)
  where obs = (`elem` obstacles s)
        inRange (x, y) = x >= 0 && y >= 0 && x <= 38 && y <= 24
        doMove to = let newGoal = if to == goal s then emptyN s else goal s
                    in s { emptyN = to, goal = newGoal}
-- neighbours (nodes, goal) = Set.fromList $ concatMap moves nodes
--   where
--     nodes' = M.fromList nodes
--     moves (node, usage) = mapMaybe (move node usage) (plane node)
--     move fromNode fromUsage toNode =
--       case M.lookup toNode nodes' of
--         Just toUsage
--           | viable (fromNode, fromUsage) (toNode, toUsage) ->
--             Just $
--             ( M.toList $
--               M.insert fromNode (emptied fromUsage) $
--               M.insert toNode (moved (used fromUsage) toUsage) $ nodes'
--             , if fromNode == goal
--                 then toNode
--                 else goal)
--         _ -> Nothing

heuristic :: Situation -> Int
heuristic s@Situation {goal = (x, y)} = x + y + distance (goal s) (emptyN s)

distance (x, y) (x', y') = abs (x - x') + abs (y - y')

isGoal :: Situation -> Bool
isGoal s = goal s == (0, 0)

plane (x,y) = [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]
