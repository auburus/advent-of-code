module Main where

import System.IO
import Data.List
import Data.List.Split

repeat' :: Int -> String -> String
repeat' 0 _ = []
repeat' nRep str = str ++ repeat' (nRep - 1) str

decompress :: String -> String
decompress str 
    | '(' `notElem` str = str
    | otherwise =
        let (uncompressed, other) = span (/='(') str
            marker = (tail . takeWhile (/=')')) other
            other' = (tail . dropWhile (/=')')) other
            [len, nRep] = map read (splitOn "x" marker)
            (toRepeat, remaining) = splitAt len other'
        in
            uncompressed ++ (repeat' nRep toRepeat) ++ decompress remaining
        

main = do
    contents <- readFile "input9.txt"
    let contents' = "HHHH(6x2)(1x3)A(3x3)XYZ"
        input = (head . lines) contents

    print $ (length . decompress) input
