module Main where

import System.IO

type Triangle = (Int, Int, Int)

validTriangle :: Triangle -> Bool
validTriangle (a,b,c) 
    | a + b <= c = False
    | a + c <= b = False
    | b + c <= a = False
    | otherwise = True
    
lineToTriangle :: String -> Triangle
lineToTriangle = tuplify . (map read) . words
    where tuplify :: [Int] -> Triangle
          tuplify (a:b:c:_) = (a,b,c)

correctTriangles :: [Triangle] -> Int
correctTriangles = foldl (\correct triangle -> if validTriangle triangle then correct+1 else correct) 0

main = do
    contents <- readFile "input3.txt"
    let countCorrect :: String -> Int
        countCorrect = correctTriangles . map lineToTriangle . lines
        countCorrect' = length . filter validTriangle . map lineToTriangle . lines

    print $ countCorrect contents
    print $ countCorrect' contents
    print $ length (lines contents)
