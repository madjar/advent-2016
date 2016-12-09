#!/usr/bin/env stack
-- stack --install-ghc runghc --package trifecta

import Text.Trifecta
import Control.Applicative
import Data.Maybe

data Compressed = Normal String | Repeated Integer String deriving Show

parser :: Parser [Compressed]
parser = some (normal <|> marker)
  where normal = Normal <$> some upper
        marker = do char '('
                    nbChar <- natural
                    char 'x'
                    repetitions <- natural
                    char ')'
                    repeated <- count (fromInteger nbChar) anyChar
                    return (Repeated repetitions repeated)

main = print . length . decompress . fromJust =<< parseFromFile parser "input9"

decompress :: [Compressed] -> String
decompress = concatMap decompress'

decompress' (Normal s) = s
decompress' (Repeated n s) = concat (replicate (fromInteger n) s)
