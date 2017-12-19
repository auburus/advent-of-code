module Main where

import System.IO (readFile)
import Data.List
import Data.Array (Array)
import qualified Data.Array as A

main = do
    contents <- readFile "input13.txt"
    let input = lines $ contents

    print . doPart1 $ input
    print . doPart2 $ input

doPart1 :: [String] -> Int
doPart1 input =
    sum .
    zipWith (\(a,b) c -> if c then a*b else 0) (ranges input) $ caughts
    where
        caughts = map (caught 0) . ranges $ input

doPart2 :: [String] -> Int
doPart2 input =
    fst . head .
    dropWhile (\(_,b) -> b) .
    zip [0, 1..] .
    map (isCaught input) $ [0, 1..]

isCaught :: [String] -> Int -> Bool
isCaught input delay = 
    or caughts
    where
        caughts = map (caught delay) . ranges $ input

ranges :: [String] -> [(Int, Int)]
ranges xs = 
    A.assocs $
    A.listArray (0, fst . last $ r) [0, 0..]
    A.// r
    where
        r = map parseRange xs

parseRange :: String -> (Int, Int)
parseRange s = (a, b)
    where
        a = read . takeWhile (/=':') $ s
        b = read . tail . dropWhile (/= ' ') $ s

caught :: Int -> (Int, Int) -> Bool
caught delay (depth, range)
    | range == 0 = False
    | (depth + delay) `mod` (2*range - 2) == 0 = True
    | otherwise = False

