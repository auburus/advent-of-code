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

splitDirections :: String -> (String, String)
splitDirections xs = (even xs, odd xs)
    where
        even [] = []
        even (x:_:xs) = x : even xs

        odd [] = []
        odd (_:y:xs) = y : odd xs

main = do
    contents <- readFile "input3.txt"
    let contents' = "^>v<"
        directions = (head . lines) contents
        (santaDir, robosantaDir) = splitDirections directions
        uniqueHouses = unique $ (houses santaDir) ++ (houses robosantaDir)

    print $ length uniqueHouses
