module Main where

import System.IO (readFile)

codeChars :: String -> Int
codeChars = length

memoryChars :: String -> Int
memoryChars str = (length (reduce str)) - 2
    where
        reduce [] = []
        reduce (x:xs)
            | x /= '\\' = x : reduce xs
            | head xs == 'x' = '.' : (reduce $ drop 3 xs)
            | otherwise = (head xs) : (reduce $ drop 1 xs)

encodedChars :: String -> Int
encodedChars str = (length (encode str)) + 2
    where
        encode [] = []
        encode (x:xs)
            | x == '"' = '\\':'"': encode xs
            | x == '\\' = '\\':'\\':encode xs
            | otherwise = x : encode xs

problem1 :: [String] -> Int
problem1 l = sum1 - sum2
    where
        sum1 = sum . map codeChars $ l
        sum2 = sum . map memoryChars $ l

problem2 :: [String] -> Int
problem2 l = sum2 - sum1
    where
        sum2 = sum . map encodedChars $ l
        sum1 = sum . map codeChars $ l

main = do
    contents <- readFile "input8.txt"
    let contents' = "\"\"\n\"abc\"\n\"aaa\\\"aaa\"\n\"\\x27\""
        input = lines contents

    print . problem1 $ input
    print . problem2 $ input
