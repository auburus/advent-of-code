module Main where

import System.IO

type House = (Int, Int)

houses :: String -> [House]
houses directions = (0,0) : zipWith nextHouse (houses directions) directions
    where
        nextHouse :: (Int, Int) -> Char -> (Int, Int)
        nextHouse (x, y) dir
            | dir == '^' = (x, y + 1)
            | dir == '>' = (x + 1, y)
            | dir == 'v' = (x, y - 1)
            | dir == '<' = (x - 1, y)
            | otherwise = error "Invalid char"

unique :: [House] -> [House]
unique [] = []
unique (x:xs) = x : unique (filter (/= x) xs)

main = do
    contents <- readFile "input3.txt"
    let contents' = "^>v<"
        uniqueHouses = (unique . houses . head . lines) contents

    print $ length uniqueHouses
