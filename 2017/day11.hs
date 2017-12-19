module Main where

import System.IO (readFile)
import Data.List
import Data.List.Split (splitOn)


main = do
    contents <- readFile "input11.txt"

    let input = splitOn "," . head . lines $ contents

    print . doPart1 $ input
    print . doPart2 $ input

doPart1 :: [String] -> Int
doPart1 = 
    abs .
    (\(x, y) -> x + y) .
    foldl (\(x, y) (a, b) -> (x+a, y+b)) (0, 0) .
    map toCoords

doPart2 :: [String] -> Int
doPart2 =
    maximum .
    map (\(x, y) -> abs(x + y)) .
    scanl (\(x, y) (a, b) -> (x+a, y+b)) (0, 0) .
    map toCoords


toCoords :: String -> (Int, Int)
toCoords dir 
    | dir == "n" = (0, 1)
    | dir == "s" = (0, -1)
    | dir == "ne" = (1, 0)
    | dir == "sw" = (-1, 0)
    | dir == "nw" = (-1, 1)
    | dir == "se" = (1, -1)
