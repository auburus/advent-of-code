module Main where

import System.IO
import Data.List

enoughVowels :: String -> Bool
enoughVowels str = length (filter isVowel str) >= 3
    where
        isVowel = \x -> x `elem` "aeiou"

doubleLetter :: String -> Bool
doubleLetter [] = False
doubleLetter (x:[]) = False
doubleLetter (x:s@(y:z))
    | x == y = True
    | otherwise = doubleLetter s

containsAny :: [String] -> String -> Bool
containsAny [] str = False
containsAny (x:xs) str 
    | isInfixOf x str = True
    | otherwise = containsAny xs str 

notContains :: [String] -> String -> Bool
notContains forbidden word = not (containsAny forbidden word)

isNice :: String -> Bool
isNice str =
    (enoughVowels str) &&
    (doubleLetter str) &&
    (notContains ["ab", "cd", "pq", "xy"] str)

main = do
    contents <- readFile "input5.txt"
    let contents' = "ugknbfddgicrmopn\naaa\njchzalrnumimnmhp\nhaegwjzuvuyypxyu\ndvszwmarrgswjxmb"
        strings = lines contents
        nice = filter isNice strings

    print $ length nice
