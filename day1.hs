#!/usr/bin/env stack
-- stack --install-ghc runghc --package trifecta

import Text.Trifecta
import Control.Applicative

main = do
  Just input <- parseFromFile parser "input1"
  let allPos = map snd . scanl (flip applyInstruction) (N, (0, 0)) $ (makeAtomic input)
  print allPos
  --print (firstDouble allPos)
  putStr "First question: "
  print (manhattan (last allPos))
  putStr "Second question: "
  print (manhattan (firstDouble allPos))


makeAtomic = concatMap (atomic)
  where atomic (d, n) = (d, 1) : replicate (fromInteger n - 1) (No, 1)

data Direction = LeftD | RightD | No deriving (Show, Eq)

type Instruction = (Direction, Integer)

parser :: Parser [Instruction]
parser = instruction' `sepBy` (symbol ", ")
  where instruction' = (,) <$> direction <*> integer
        direction = (char 'L' *> pure LeftD)
                <|> (char 'R' *> pure RightD)

type Pos = (Integer, Integer)
data CurrentDir = N | E | S |W deriving (Show, Eq, Enum)

type State = (CurrentDir, Pos)

applyInstruction :: Instruction -> State -> State
applyInstruction (dir, steps) = advance steps . rotate dir

rotate :: Direction -> State -> State
rotate dir (c, p) = (rotated dir c, p)

rotated :: Direction -> CurrentDir -> CurrentDir
rotated d = (toEnum . (\ n -> (n + modif + 4) `mod` 4 ). fromEnum)
  where modif = case d of
          LeftD -> -1
          RightD -> 1
          No -> 0

advance :: Integer -> State -> State
advance n (c, (x, y)) = (c, case c of
                            N -> (x, y+n)
                            E -> (x+n, y)
                            S -> (x, y-n)
                            W -> (x-n, y)
                            )

manhattan (x, y) = abs x + abs y

firstDouble [] = error "no doubles in list"
firstDouble (x:xs) = if x `elem` xs
                       then x
                       else firstDouble xs
