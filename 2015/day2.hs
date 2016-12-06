module Main where

import System.IO
import Data.List.Split
import qualified Data.Char as C

type Present = (Int, Int, Int)

parseInput :: String -> Present
parseInput = tuplify . map read . splitOn "x" 
    where
        tuplify [a,b,c] = (a,b,c)

surface :: Present -> Int
surface (w, l, h) = 2*w*l + 2*l*h + 2*h*w

extra :: Present -> Int
extra = minimum . map tupleProduct . combinations
    where
        tupleProduct (a,b) = a*b

combinations :: Present -> [(Int, Int)]
combinations (w, l, h) = [(w,l), (l,h), (h,w)]

wrapRibbon :: Present -> Int
wrapRibbon = minimum . map perimeter . combinations
    where
        perimeter (a,b) = 2*a + 2*b

bow :: Present -> Int
bow (a,b,c) = a*b*c

main = do
    contents <- readFile "input2.txt"
    let contents' = "2x3x4\n1x1x10"
        presents = map parseInput $ lines contents
        paper = zipWith (+) (map surface presents) (map extra presents)
        ribbon = zipWith (+) (map wrapRibbon presents) (map bow presents)

    print $ foldl (+) 0 paper
    print $ foldl (+) 0 ribbon
