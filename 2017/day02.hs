module Main where

import System.IO
import Data.List
import Data.List.Split (splitOn)

main = do
    contents <- readFile "input02.txt"
    let contents' = ""
        input = map (map read . words) . lines $ contents :: [[Int]] 

    print . doPart1 $ input
    print . doPart2 $ input

doPart1 :: [[Int]] -> Int
doPart1 input = let
    maxs = map maximum input
    mins = map minimum input

    difs = zipWith (\a b -> a - b) maxs mins

    in
        foldl (+) 0 difs

doPart2 :: [[Int]] -> Int
doPart2 = 
    foldl (+) 0 .
    map (\(a, b) -> if a > b then a `div` b else b `div` a) .
    map findDivisiblePair 

findDivisiblePair :: [Int] -> (Int, Int)
findDivisiblePair list = 
    head .
    map (\(xs, a) -> (head xs, a)) .
    filter (\(xs, _) -> not (null xs)) .
    map isDivisible  .
    zip (repeat list) $ list

isDivisible :: ([Int], Int) -> ([Int], Int)
isDivisible (xs,a) = (filter (\x -> (x `mod` a == 0) && (x /= a)) xs, a)

    
