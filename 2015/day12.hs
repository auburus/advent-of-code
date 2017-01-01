module Main where

import System.IO (readFile)
import Data.Char (intToDigit)

validChars = '-' : (map intToDigit [0..9])

nums :: String -> [Int]
nums [] = []
nums str
    | num == [] = []
    | otherwise = (read num) : (nums rem)
    where
        firstValid = dropWhile (\x -> notElem x validChars) str
        (num, rem) = span (\x -> elem x validChars) firstValid

objects :: String -> [String]
objects [] = []
objects str
    | firstValid == [] = []
    | otherwise = object : ((objects object) ++ (objects rem))
    where
        firstValid = dropWhile (/='{') $ str
        (object, rem) = span (/='}') . tail $ firstValid

printList [] = return ()
printList (x:xs) = do
    print x
    putStrLn ""
    printList xs

main = do
    contents <- readFile "input12.txt"
    let input = head . lines $ contents

    print . sum . nums $ input

    putStrLn ""
    printList . objects $ input
