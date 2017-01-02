module Main where

import System.IO (readFile)
import Data.List


problem1 :: [Int] -> Int
problem1 = length .
           filter (\xs -> sum xs == 150) .
           subsequences

problem2 :: [Int] -> Int
problem2 cont = length .
                filter (\x -> length x == minLength) $
                subseq
    where
        subseq = filter (\xs -> sum xs == 150) . subsequences $ cont
        minLength = minimum . map length $ subseq

main = do
    contents <- readFile "input17.txt"
    let input :: [Int]
        input = map read . lines $ contents

    print . problem1 $ input
    print . problem2 $ input
