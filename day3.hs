#!/usr/bin/env stack
-- stack --install-ghc runghc --package trifecta

import Text.Trifecta
import Control.Applicative
import Data.Maybe
import Data.List.Extra

data Triangle = Triangle Integer Integer Integer deriving Show

parser :: Parser [Triangle]
parser = whiteSpace *> some triangle
  where triangle = liftA3 Triangle integer integer integer

parser' :: Parser [Triangle]
parser' = whiteSpace *> (concat <$> some threeTriangles)
  where threeTriangles = map mkTriangle . transpose . chunksOf 3 <$> sequenceA (replicate 9 integer)
        mkTriangle [a, b, c] = Triangle a b c

isValid (Triangle a b c) = let [x, y, z] = sort [a, b, c]
                           in x + y > z

main = print . length . filter isValid . fromJust=<< parseFromFile parser' "input3"
