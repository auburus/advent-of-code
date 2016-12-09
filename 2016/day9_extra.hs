module Main where

import System.IO
import Data.List
import Data.List.Split

repeat' :: Int -> String -> String
repeat' 0 _ = []
repeat' nRep str = str ++ repeat' (nRep - 1) str

decompressLen :: String -> Integer
decompressLen str 
    | '(' `notElem` str = (toInteger . length) str
    | otherwise =
        let (uncompressed, other) = span (/='(') str
            marker = (tail . takeWhile (/=')')) other
            other' = (tail . dropWhile (/=')')) other
            [len, nRep'] = map read (splitOn "x" marker)
            nRep = toInteger nRep'
            (toRepeat, remaining) = splitAt len other'
        in
            ((toInteger . length) uncompressed) + nRep * (decompressLen toRepeat) + decompressLen remaining

main = do
    contents <- readFile "input9.txt"
    let contents' = "(27x12)(20x12)(13x14)(7x10)(1x12)A"
        input = (head . lines) contents

    print $ decompressLen input
