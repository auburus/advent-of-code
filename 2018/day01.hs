module Main where

import System.IO
import Data.List
import Data.Maybe (catMaybes)
import Control.Applicative

main = do
    contents <- readFile "input01.txt"
    input <- map (myRead) . lines
          <$> readFile "input01.txt"

    print . sum $ input
    print . problem2 $ input
    where
        myRead ('+':xs) = read xs
        myRead xs = read xs

partialSums :: Int -> [Int] -> [Int]
partialSums a [] = [a]
partialSums a (x:xs) = a : (partialSums (a+x) xs)

problem2 :: [Int] -> Int
problem2 input =
    let m = catMaybes
          . closestMultiple 0
          . tail
          . partialSums 0
          $ input
        min' = minimum . snd . unzip $ m
    in
        fst . head . filter ((==min') . snd ) $ m

closestMultiple :: Int -> [Int] -> [Maybe (Int, Int)]
closestMultiple i xs 
    | i >= length xs = []
    | otherwise = (findMultiples (xs !! i) offset (delete (xs !! i) xs)) : closestMultiple (i+1) xs
    where
        offset = last xs

findMultiples :: Int -> Int -> [Int] -> Maybe (Int, Int)
findMultiples a offset list =
    let matches = filter ((>0) . snd)
                . map (\x -> (x, (x-a) `div` offset))
                . filter ((==0) . (\x -> mod x offset) . ((-)a) )
                $ list
    in
        case matches of
            [] -> Nothing
            _ -> Just $ minimumBy (\a b -> compare (snd a) (snd b)) matches
