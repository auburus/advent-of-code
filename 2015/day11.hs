module Main where

import Data.Char (ord, chr)
import Data.List

next :: String -> String
next = reverse . map (\x -> if x == '_' then 'a' else x) . increment . reverse

increment :: String -> String
increment [] = error "Something happened"
increment (x:xs)
    | x == 'z' = 'a' : increment xs
    | otherwise = (chr . (+ 1) . ord) x : xs

possible :: String -> [String]
possible = tail . map reverse . iterate increment . reverse


hasIncr3 :: String -> Bool
hasIncr3 (x:s@(y:z:_))
    | ord x + 1 == ord y && ord y + 1 == ord z = True
    | otherwise = hasIncr3 s
hasIncr3 _ = False

notContain :: String -> Bool
notContain str = foldl (&&) True . map (\a -> notElem a str) $ "iol"

has2Pair :: String -> Bool
has2Pair (_:[]) = False
has2Pair (x:s@(y:xs))
    | x == y = hasPair xs
    | otherwise = has2Pair s

hasPair :: String -> Bool
hasPair [] = False
hasPair (_:[]) = False
hasPair (x:s@(y:_))
    | x == y = True
    | otherwise = hasPair s


nextPassword :: String -> String
nextPassword = head . filter (\x -> notContain x && has2Pair x && hasIncr3 x) . possible

main = do
    let input = "vzbxkghb"
        input2 = nextPassword input

    print input2
    print . nextPassword $ input2
