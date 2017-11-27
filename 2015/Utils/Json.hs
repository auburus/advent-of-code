module Utils.Json
( Json
, parse
) where

import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Json = JInt Int
          | JStr String
          | JList List
          | JMap Map
          | JNull
          deriving (Show)

ltrim :: String -> String
ltrim = dropWhile (==' ')

parse :: String -> (Json, String)
parse [] = (JNull, [])
parse str
    | x == '{' = JMap $ parseObject xs
    where
        (x:xs) == ltrim str

parseObject :: String -> (Map String Json, String)
parseObject str
    | x == '}' = (Map.empty, xs)
    | x == '\"' = (Map.insert key value $ (fst . parseObject) remaining, snd remaining)
    where
        (x:xs) == ltrim str
        (key, rem') = span (/='\"') xs
        (value, remaining) = parse xs
