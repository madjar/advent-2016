#!/usr/bin/env stack
-- stack --install-ghc runghc --package trifecta

import Text.Trifecta
import Control.Applicative
import qualified Data.Vector as V
import qualified Data.Map as Map
import Data.Map (Map)

data Value = Lit Int | Reg Reg deriving Show
type Reg = Char
data Inst = Copy Value Reg | Inc Reg | Dec Reg | Jnz Value Int deriving Show

parser :: Parser (V.Vector Inst)
parser = V.fromList <$> many (copy <|> inc <|> dec <|> jnz)
  where
    copy = Copy <$ symbol "cpy" <*> value <*> reg
    inc = Inc <$ symbol "inc" <*> reg
    dec = Dec <$ symbol "dec" <*> reg
    jnz = Jnz <$ symbol "jnz" <*> value <*> int
    reg = token lower
    int = fromInteger <$> integer
    value = Lit <$> int <|> Reg <$> reg

main = do
  Just input <- parseFromFile parser "input12"
  print (run input)


type State = (Int, Map Reg Int)

run :: V.Vector Inst -> State
run insts = go initial
  where
    go state@(ri, _) =
      if ri < V.length insts
        then go (interpret state (insts V.! ri))
        else state

initial = (0, Map.fromList [('a', 0), ('b', 0), ('c', 1), ('d', 0)])


interpret :: State -> Inst -> State
interpret (ri, reg) i =
  case i of
    Copy v dest -> (ri + 1, Map.insert dest (compute v) reg)
    Inc r -> (ri + 1, Map.adjust (+ 1) r reg)
    Dec r -> (ri + 1, Map.adjust (+ (-1)) r reg)
    Jnz v offset ->
      if compute v /= 0
        then (ri + offset, reg)
        else (ri + 1, reg)
  where
    compute (Lit i) = i
    compute (Reg r) = reg Map.! r
