module Main where

import System.IO
import Data.List

doublePair :: String -> Bool
doublePair [] = False
doublePair (x:[]) = False
doublePair (x:s@(y:xs))
    | pair `isInfixOf` xs = True
    | otherwise = doublePair s
    where
        pair = x : y : ""

repeated :: String -> Bool
repeated [] = False
repeated (_:[]) = False
repeated (_:_:[]) = False
repeated (x:s@(_:z:_)) 
    | x == z = True
    | otherwise = repeated s

isNice :: String -> Bool
isNice str =
    (doublePair str) &&
    (repeated str)

main = do
    contents <- readFile "input5.txt"
    let contents' = "qjhvhtzxzqqjkmpb\nxxyxx\nuurcxstgmygtbstg\nieodomkazucvgmuy"
        strings = lines contents
        nice = filter isNice strings

    print $ length nice
