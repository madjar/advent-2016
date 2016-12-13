#!/usr/bin/env stack
-- stack --install-ghc runghc --package heap

import Data.List
import Control.Applicative
import Control.Monad
import Data.Array
import Debug.Trace
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Heap as Heap

-- The first floor contains a thulium generator, a thulium-compatible microchip, a plutonium generator, and a strontium generator.
-- The second floor contains a plutonium-compatible microchip and a strontium-compatible microchip.
-- The third floor contains a promethium generator, a promethium-compatible microchip, a ruthenium generator, and a ruthenium-compatible microchip.
-- The fourth floor contains nothing relevant.

data Type = Generator | Microchip deriving (Show, Eq, Ord)
data Elem
  = Thulium
  | Plutonium
  | Strontium
  | Promethium
  | Ruthenium
  | Elerium
  | Dilithium
  deriving (Show, Eq, Ord)

type Floor = [(Type, Elem)]

isGenerator = (== Generator) . fst
isChip = (== Microchip) . fst

input =
  listArray
    (1, 4)
    [ [ (Generator, Thulium)
      , (Microchip, Thulium)
      , (Generator, Plutonium)
      , (Generator, Strontium)
      , (Generator, Elerium)
      , (Microchip, Elerium)
      , (Generator, Dilithium)
      , (Microchip, Dilithium)
      ]
    , [(Microchip, Plutonium), (Microchip, Strontium)]
    , [ (Generator, Promethium)
      , (Microchip, Promethium)
      , (Generator, Ruthenium)
      , (Microchip, Ruthenium)
      ]
    , []
    ]

type State = (Integer, Array Integer Floor)

main = print $ solve (1, input)

printFloors :: Array Integer Floor -> IO ()
printFloors = mapM_ print

solve initial = solve' Set.empty (Heap.singleton (0, (0, initial)))

solve' :: Set State -> Heap.MinPrioHeap Int (Int, State) -> Int
solve' seen candidates =
  if isSolved state
    then depth
    else traceShow (depth, length seen) $
         solve' (Set.insert state seen) newCandidates
  where
    Just ((_, (depth, state)), otherCandidates) =
      Heap.view $
      Heap.dropWhile (\(_, (d, s)) -> s `Set.member` seen) candidates
    steps = nextSteps state
    newCandidates =
      foldr Heap.insert otherCandidates . map (\s -> (cost s, (depth + 1, s))) $
      steps
    cost (elevator, step) =
      sum . map (\(level, content) -> (fromInteger $ 4 - level) * length content) . assocs $
      step

isSolved :: State -> Bool
isSolved = all (\(i, f) -> i == 4 || null f). assocs . snd

nextSteps :: State -> [State]
nextSteps (elevator, floors) = do
  nextFloor <- [elevator + 1, elevator - 1]
  guard (bounds floors `inRange` nextFloor)
  (toMove, toStay) <- pickOneOrTwo (floors ! elevator)
  let destinationFloor = sort (toMove ++ (floors ! nextFloor))
      newFloors = floors // [(elevator, toStay), (nextFloor, destinationFloor)]
  guard (isLegal destinationFloor)
  return (nextFloor, newFloors)


isLegal :: Floor -> Bool
isLegal floor = noGenerator floor || allProtected floor
  where
    noGenerator = all (not . isGenerator)
    allProtected floor =
      all (\(_, e) -> (Generator, e) `elem` floor) $ filter isChip floor

pickOne :: Eq a => [a] -> [(a, [a])]
pickOne l = do
  e <- l
  return (e, delete e l)

pickOneOrTwo :: (Ord a, Eq a) => [a] -> [([a], [a])]
pickOneOrTwo l = do
  (e, rest) <- pickOne l
  return ([e], rest) <|> do (e', rest') <- pickOne rest
                            guard (e >= e')
                            return ([e, e'], rest')
