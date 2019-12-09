module Day01 where

import System.IO

main = do
    input <- readFile "input01.txt"
    print $ problem1 input 
    print $ problem2 input 


problem1 input = sum . map (fuel . read) . lines $ input
problem2 input = sum . map (totalFuel . read) . lines $ input

fuel :: Integer -> Integer
fuel x = max ((x `div` 3) - 2) 0

totalFuel :: Integer -> Integer
totalFuel x = doTotal x 0
    where
        doTotal x sum 
            | x <= 0 = sum
            | otherwise = doTotal (fuel x) (sum + fuel x)
