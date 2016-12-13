#!/usr/bin/env stack
-- stack --install-ghc runghc --package trifecta --package raw-strings-qq
{-# LANGUAGE QuasiQuotes #-}

import Text.Trifecta
import Control.Applicative
import Data.Maybe
import Text.RawString.QQ
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List
import Debug.Trace

type Bot = Int
data Dest = Bot Bot | Output Int deriving (Show, Eq, Ord)
data Inst = Initial Int Bot | Gives Bot Dest Dest deriving Show

main = do
  --let Success input = parseString parser mempty example
  Just input <- parseFromFile parser "input10"
  let solved = solve input
  print solved
  putStr "Responsible for 61 and 17: "
  print (findResponsible 61 17 solved)
  putStrLn "Product of output 0, 1 and 2"
  let get x = head (solved Map.! Output x)
  print (get 0 * get 1 * get 2)

example = [r|value 5 goes to bot 2
bot 2 gives low to bot 1 and high to bot 0
value 3 goes to bot 1
bot 1 gives low to output 1 and high to bot 0
bot 0 gives low to output 2 and high to output 0
value 2 goes to bot 2
|]

parser = some (initial <|> gives)
  where
    initial = Initial <$ symbol "value" <*> int <* symbol "goes to bot" <*> int
    gives =
      Gives <$ symbol "bot" <*> int <* symbol "gives low to" <*> dest <*
      symbol "and high to" <*>
      dest
    dest = Bot <$ symbol "bot" <*> int <|> Output <$ symbol "output" <*> int
    int = fromInteger <$> integer

solve :: [Inst] -> Map Dest [Int]
solve instructions = finalMap
  where
    finalMap = foldl process Map.empty instructions
    process state (Initial value bot) = add (Bot bot) value state
    process state (Gives bot lowD highD) =
      let [lowV, highV] = sort (finalMap Map.! (Bot bot))
      in add lowD lowV . add highD highV $ state

    add k v = Map.insertWith (++) k [v]

findResponsible :: Int -> Int -> Map Dest [Int] -> Bot
findResponsible x y = extract . find rightBot . Map.toList
  where rightBot (k, v) = v == [x, y] || v == [y, x]
        extract (Just (Bot b, _)) = b
