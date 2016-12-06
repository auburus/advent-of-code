module Main where

import Data.List
import System.IO

mostCommon :: String -> Char
mostCommon = fst . last . sortBy sorted . ocurrences
    where
        sorted (_, val1) (_, val2) = compare val2 val1


ocurrences :: String -> [(Char, Int)]
ocurrences [] = []
ocurrences s@(x:xs) = (x, length $ foundChars) : ocurrences others
    where (foundChars, others) = partition (==x) s

wordsToCols :: [String] -> [String]
wordsToCols [] = []
wordsToCols strings@(x:xs) 
    | null x = []
    | otherwise = (map head strings) : wordsToCols (map tail strings)

main = do
    contents <- readFile "input6.txt"
    let contents' = "abc\nbca\nbca"
        words = lines contents

    print $ map mostCommon (wordsToCols words)
