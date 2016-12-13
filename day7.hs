{-# LANGUAGE TupleSections #-}
#!/usr/bin/env stack
-- stack --install-ghc runghc --package trifecta

import Text.Trifecta
import Control.Applicative
import Data.Maybe
import Data.List

data Section = Normal String | Hyper String deriving Show
-- Some ugly boilerplate
getStr (Normal s) = s
getStr (Hyper s) = s
getNormal (Normal s) = Just s
getNormal _ = Nothing
getHyper (Hyper s) = Just s
getHyper _ = Nothing

type IP = [Section]

parser :: Parser [IP]
parser = some section `endBy1` newline
  where section = Normal <$> some lower
              <|> Hyper <$> between (char '[') (char ']') (some lower)

main = do
  Just input <- parseFromFile parser "input7"
  putStr "First: "
  print . length . filter hasTls $ input
  putStr "Second: "
  print . length . filter hasSSL $ input

hasAbba :: String -> Bool
hasAbba = any isAbba . window 4
  where isAbba [a, b, c, d] = a == d && a /=c && b == c

data Status = No | Abba | AbbaInHyper deriving Eq

hasTls :: IP -> Bool
hasTls =  isValid . map toStatus
  where toStatus s | hasAbba (getStr s) = case s of
                       Normal _ -> Abba
                       Hyper _ -> AbbaInHyper
        toStatus _ = No
        isValid s = any (== Abba) s && all (/= AbbaInHyper) s

getAbas :: String -> [String]
getAbas = filter isAba . window 3
  where isAba [a, b, c] = a /= b && a == c

hasSSL :: IP -> Bool
hasSSL ip = not (null (normAbas `intersect` normBabs))
  where abas = concatMap getAbas . catMaybes . map getNormal $ ip
        babs = concatMap getAbas . catMaybes . map getHyper $ ip
        normAbas = map (\[a, b, _] -> (a, b)) abas
        normBabs = map (\[b, a, _] -> (a, b)) babs

window n xs =
  if (length xs) < n
    then []
    else (take n xs) : window n (tail xs)
