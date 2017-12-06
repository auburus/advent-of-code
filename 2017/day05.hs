module Main where

import System.IO
import Data.List
import Data.Array (Array)
import qualified Data.Array as A


main = do
    contents <- readFile "input05.txt"
    let input = [0, 3, 0, 1, -3]
        input' = map read . lines $ contents :: [Int]
        

    print . doPart1 $ input'
    print . doPart2 $ input'

doPart1 :: [Int] -> Int
doPart1 input =
    let instructions = A.listArray (0, length input - 1) input
    in
        move instructions 0 0

doPart2 :: [Int] -> Int
doPart2 input =
    let instructions = A.listArray (0, length input - 1) input
    in
        move' instructions 0 0

move' :: Array Int Int -> Int -> Int -> Int
move' instructions moves index
    | index > maxB || index < minB = moves
    | otherwise =
        move' instructions' (moves+1) nextIndex
    where
        (minB, maxB) = A.bounds instructions
        current = instructions A.! index
        instructions'
            | current >= 3 = instructions A.// [(index, current - 1)]
            | otherwise =  instructions A.// [(index, current + 1)]
        nextIndex = index + current

move :: Array Int Int -> Int -> Int -> Int
move instructions moves index
    | index > maxB || index < minB = moves
    | otherwise =
        move instructions' (moves+1) nextIndex
    where
        (minB, maxB) = A.bounds instructions
        instructions' = instructions A.// [(index, (instructions A.! index) + 1)]
        nextIndex = index + (instructions A.! index)
