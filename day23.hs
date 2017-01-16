{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
#!/usr/bin/env stack
-- stack --install-ghc runghc --package trifecta --package lens --package monad-loops

import Text.Trifecta
import Control.Applicative
import qualified Data.Vector as V
import Data.Vector (Vector)
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Lens
import Control.Monad.State
import Control.Monad.Loops
import Data.Maybe
import Debug.Trace

data Value = Lit Integer | Reg Reg deriving Show
type Reg = Char
data Inst
  = Copy Value
         Value
  | Inc Reg
  | Dec Reg
  | Jnz Value
        Value
  | Toggle Value
  | Invalid
  deriving (Show)

data Memory = Memory
  { _ri :: Integer
  , _reg :: Map Reg Integer
  , _insts :: Vector Inst
  } deriving (Show)
makeLenses ''Memory


parser :: Parser (V.Vector Inst)
parser = V.fromList <$> many (copy <|> inc <|> dec <|> jnz <|> tgl)
  where
    copy = Copy <$ symbol "cpy" <*> value <*> value
    inc = Inc <$ symbol "inc" <*> reg
    dec = Dec <$ symbol "dec" <*> reg
    jnz = Jnz <$ symbol "jnz" <*> value <*> value
    tgl = Toggle <$ symbol "tgl" <*> value
    reg = token lower
    value = Lit <$> integer <|> Reg <$> reg

main = do
  Just input <- parseFromFile parser "input23"
  print . execState run . mkMemory $ input

run :: State Memory ()
run = whileJust_ currentInst interpret
  where
    currentInst = do
      r <- use ri
      preuse (insts . ix (fromInteger r))

mkMemory :: Vector Inst -> Memory
mkMemory input =
  Memory 0 (Map.fromList [('a', 7), ('b', 0), ('c', 1), ('d', 0)]) input

interpret :: Inst -> State Memory ()
interpret i = (use reg >>= traceShowM) >> (traceShowM i) >>
  case i of
    Copy v (Reg dest) -> do reg . ix dest <~ compute v; advance
    Copy _ (Lit _) -> advance
    Inc r -> do reg . ix r += 1; advance
    Dec r -> do reg . ix r -= 1; advance
    Jnz v offsetV -> do
      value <- compute v
      offset <- compute offsetV
      if value /= 0
        then when (offset == 0) (fail "loop detected") >> ri += offset
        else advance
    Toggle offsetV -> do
      offset <- compute offsetV
      (insts . ix (fromInteger offset) %= toggle)
      advance
    Invalid -> advance
  where
    compute :: Value -> State Memory Integer
    compute (Lit i) = return i
    compute (Reg r) = use (reg . at r . to fromJust)
    advance = ri += 1

toggle :: Inst -> Inst
toggle i = case  i of
  Inc r -> Dec r
  Dec r -> Inc r
  Jnz v o -> Copy v o
  Copy v dest -> Jnz v dest
  Toggle (Lit offset) -> Invalid
  Toggle (Reg r) -> Inc r
  Invalid -> Invalid
