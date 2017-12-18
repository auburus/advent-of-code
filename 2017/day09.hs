
module Main where

import System.IO
import Data.List

main = do
    contents <- readFile "input09.txt"
    let input = head . lines $ contents     

    print . doPart1 $ input
    print . doPart2 $ input


doPart1 :: String -> Int
doPart1 input = processString 0 input

doPart2 :: String -> Int
doPart2 input = countGarbage $ input

processString :: Int -> String -> Int
processString _ [] = 0
processString i (x:xs)
    | x == '{' = (i+1) + processString (i+1) xs
    | x == '}' = processString (i-1) xs
    | x == '<' = processString i $ dropGarbage xs
    | x == '!' = processString i $ tail xs
    | otherwise = processString i xs

dropGarbage :: String -> String
dropGarbage (x:xs)
    | x == '>' = xs
    | x == '!' = dropGarbage $ tail xs
    | otherwise = dropGarbage xs

countGarbage :: String -> Int
countGarbage [] = 0
countGarbage (x:xs)
    | x == '<' = i + countGarbage xs'
    | x == '!' = countGarbage $ tail xs
    | otherwise = countGarbage xs
    where
        (i, xs') = dropGarbage' 0 xs

dropGarbage' :: Int -> String -> (Int, String)
dropGarbage' i (x:xs)
    | x == '>' = (i, xs)
    | x == '!' = dropGarbage' i $ tail xs
    | otherwise = dropGarbage' (i+1) xs
