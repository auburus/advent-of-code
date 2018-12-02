module Main where

import System.IO
import Data.List
import Data.Set (Set)
import qualified Data.Set as S

main = do
    contents <- readFile "input01.txt"
    let input = map (read . (\(a:xs) -> if a == '+' then xs else a:xs))
              . lines
              $ contents :: [Int]

    print . sum $ input
    print . fmap fst . problem2 $ input

partialSums :: Int -> [Int] -> [Int]
partialSums a [] = [a]
partialSums a (x:xs) = a : (partialSums (a+x) xs)

problem2 :: [Int] -> Maybe (Int, Int)
problem2 input =
    let m = myMap 0
          . tail . partialSums 0 $ input
        min' = minimum' m
    in
        head . filter ((==min') . maybe (-1) snd ) $ m

myMap :: Int -> [Int] -> [Maybe (Int, Int)]
myMap i xs 
    | i >= length xs = []
    | otherwise = (findMultiples (xs !! i) (last xs) (delete (xs !! i) xs)) : myMap (i+1) xs

findMultiples :: Int -> Int -> [Int] -> Maybe (Int, Int)
findMultiples a offset list =
    let matches = filter ((>0) . snd)
                . map (\x -> (x, (x-a) `div` offset))
                . filter ((==0) . (\x -> mod x offset) . ((-)a) )
                $ list
    in
        case matches of
            [] -> Nothing
            _ -> Just $ foldl min' (head matches) (tail matches)

min' :: (Int, Int) -> (Int, Int) -> (Int, Int)
min' (a,b) (c,d)
    | d < b = (c,d)
    | otherwise = (a,b)

minimum' :: [Maybe (Int, Int)] -> Int
minimum' xs = foldl (\b x -> maybe b (min b . snd) x) (maybe 0 snd (head  xs)) (tail xs)
