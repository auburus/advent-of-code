module Main where

import System.IO
import qualified Data.Array as A
import qualified Data.Map.Strict as Map


validInstruction :: A.Array Int String -> Int -> Bool
validInstruction instructions i = i `elem` (A.indices instructions)

nextInstruction :: Map.Map String Integer -> String -> Int -> Int
nextInstruction registers instruction i
    | operation == "jnz" =
        if getValue registers fstArg == 0
        then i + 1
        else i + value
    | otherwise = i + 1

    where
        splitted = words instruction
        operation = head splitted
        fstArg = splitted !! 1
        value :: Int
        value = read (splitted !! 2)

getValue :: Map.Map String Integer -> String -> Integer
getValue registers value =
    if value `elem` ["a","b","c","d"]
    then registers Map.! value
    else read value


updateRegisters :: Map.Map String Integer -> String -> Map.Map String Integer
updateRegisters registers instruction
    | operation == "cpy" =
        Map.adjust (\_ -> getValue registers fstVal) sndVal registers
    | operation == "inc" =
        Map.adjust (\x -> x + 1) fstVal registers
    | operation == "dec" =
        Map.adjust (\x -> x - 1) fstVal registers
    | otherwise = registers

    where
        splitted = words instruction
        operation = head splitted
        fstVal = splitted !! 1
        sndVal = splitted !! 2

execute :: Map.Map String Integer -> A.Array Int String -> Int -> Map.Map String Integer
execute registers instructions i
    | not $ validInstruction instructions i = registers
    | otherwise =
        execute registers' instructions (nextInstruction registers current i)

    where
        current = instructions A.! i
        registers' = updateRegisters registers current

registers :: Map.Map String Integer
registers = Map.fromList [("a", 0), ("b", 0), ("c", 1), ("d", 0)] 

main = do
    contents <- readFile "input12.txt"
    let contents' = "cpy 41 a\ninc a\ninc a\ndec a\njnz a 2\ndec a"
        instructions' = lines contents
        instructions = A.listArray (0, length instructions' - 1) instructions'

    print $ execute registers instructions 0
