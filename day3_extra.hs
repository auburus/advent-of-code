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

wrongTriangles :: String -> [Triangle]
wrongTriangles = map lineToTriangle . lines

-- Convert list of triangles read by row to triangles read by cols
convertTriangles :: [Triangle] -> [Triangle]
convertTriangles = splitToTriangles . trianglesToList
    where
        trianglesToList :: [Triangle] -> [Int]
        trianglesToList triangles = 
            map (\(a,_,_) -> a) triangles ++
            map (\(_,b,_) -> b) triangles ++
            map (\(_,_,c) -> c) triangles

        splitToTriangles :: [Int] -> [Triangle]
        splitToTriangles [] = []
        splitToTriangles (a:b:c:xs) = (a,b,c) : (splitToTriangles xs)

main = do
    contents <- readFile "input3.txt"
    let countCorrect :: String -> Int
        countCorrect = correctTriangles . map lineToTriangle . lines

        getGoodTriangles = convertTriangles . map lineToTriangle . lines

    print $ correctTriangles (getGoodTriangles contents)
