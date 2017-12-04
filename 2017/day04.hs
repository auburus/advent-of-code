module Main where

import System.IO
import Data.List
import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as S

main = do
    contents <- readFile "input04.txt"
    let contents' = ""
        input = map words . lines $ contents

    print . doPart1 $ input
    print . doPart2 $ input

doPart1 :: [[String]] -> Int
doPart1 input = 
    foldl (\b a -> if a then b+1 else b) 0 . 
    map validPassphrase $ input

doPart2 :: [[String]] -> Int
doPart2 input = 
    foldl (\b a -> if a then b+1 else b) 0 . 
    map validPassphrase' $ input


validPassphrase :: [String] -> Bool
validPassphrase w = 
    (length . nub $ w) == length w
        

validPassphrase' :: [String] -> Bool
validPassphrase' w = 
    let w' = map (\a -> sort a) w
    in 
        (length . nub $  w') == length w'
