module Main where

import System.IO
import Data.List
import Data.Char (digitToInt)

main = do
    contents <- readFile "input01.txt"
    let contents' = ""
        input = (head . lines) contents

    print . foldl (\sum (x,y) -> x + sum) 0
        . filter (\(x,y) -> x == y) 
        . selfPair 1
        . digitize
        $ input

    print . foldl (\sum (x,y) -> x + sum) 0
        . filter (\(x,y) -> x == y) 
        . selfPair ((length input) `div` 2)
        . digitize
        $ input


digitize :: [Char] -> [Int]
digitize = map digitToInt

selfPair :: Int -> [Int] -> [(Int, Int)]
selfPair offset xs = zip xs . drop offset . cycle $ xs
