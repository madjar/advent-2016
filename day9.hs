#!/usr/bin/env stack
-- stack --install-ghc runghc --package trifecta

import Text.Trifecta
import Control.Applicative
import Data.Maybe

data Compressed = Normal String | Repeated Integer [Compressed] deriving Show

parser :: Parser [Compressed]
parser = some (normal <|> marker)
  where normal = Normal <$> some upper
        marker = do char '('
                    nbChar <- natural
                    char 'x'
                    repetitions <- natural
                    char ')'
                    repeated <- count (fromInteger nbChar) anyChar
                    pos <- position
                    let Success parsed = parseString parser mempty repeated
                    return (Repeated repetitions parsed)

main = print . size . fromJust =<< parseFromFile parser "input9"

decompress :: [Compressed] -> String
decompress = concatMap decompress'

decompress' (Normal s) = s
decompress' (Repeated n s) = concat (replicate (fromInteger n) (decompress s))

size :: [Compressed] -> Int
size = sum . map size'

size' (Normal s) = length s
size' (Repeated n s) = (fromInteger n) * size s
