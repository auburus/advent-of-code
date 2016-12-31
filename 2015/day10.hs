module Main where

import Data.Char (digitToInt)

lockAndSay :: [Int] -> [Int]
lockAndSay [] = []
lockAndSay (x:xs) = (length equals + 1) :x : (lockAndSay others)
    where (equals, others) = span (==x) xs

problem1 = length . last . take 41 . iterate lockAndSay
problem2 = length . last . take 51 . iterate lockAndSay

main = do
    let input :: [Int]
        input = map digitToInt "1113222113"

    print . problem1 $ input
    print . problem2 $ input
