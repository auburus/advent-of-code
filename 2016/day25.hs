module Main where

import System.IO
import qualified Data.Array as A
import qualified Data.Map.Strict as Map
import Prelude hiding (seq)


validInstruction :: A.Array Integer String -> Integer -> Bool
validInstruction instructions i = i `elem` (A.indices instructions)

nextInstruction :: Map.Map String Integer -> String -> Integer -> Integer
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
        value = getValue registers (splitted !! 2)

getValue :: Map.Map String Integer -> String -> Integer
getValue registers value =
    if value `elem` ["a","b","c","d"]
    then registers Map.! value
    else read value


updateRegisters :: Map.Map String Integer -> String -> Map.Map String Integer
updateRegisters registers instruction
    | operation == "cpy" =
        if sndVal `elem` ["a","b","c","d"]
            then Map.adjust (\_ -> getValue registers fstVal) sndVal registers
            else registers
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

-- Special case. update registers accordingly
loop1 :: Map.Map String Integer -> Map.Map String Integer
loop1 reg = Map.insert "a" newVal $ Map.insert "d" newVal reg
    where
        newVal = 7 * 365 + reg Map.! "a"

loop2 :: Map.Map String Integer -> Map.Map String Integer
loop2 reg = Map.insert "a" (a `div` 2) $ Map.insert "b" (a `mod` 2) reg
    where
        a = reg Map.! "a"

execute :: Map.Map String Integer -> A.Array Integer String -> Integer -> [(Integer, Map.Map String Integer)]
execute registers instructions i
    | i == 0 = execute (loop1 registers) instructions 9
    -- | i == 10 = execute (loop2 registers) instructions 12
    | i == 27 = (27, registers) : execute registers instructions 28
    | not $ validInstruction instructions i = (-1, registers) : []
    | otherwise = 
        execute registers' instructions (nextInstruction registers current i)

    where
        current = instructions A.! i
        registers' = updateRegisters registers current

registers = Map.fromList [("a", 175), ("b", 0), ("c", 0), ("d", 0)]

-- The equivalent problem is finding the first int that its sequence is 0,1,0,1..
seq :: Int -> [Int]
seq seed = doSeq $ 7 * 365 + seed
    where
        doSeq i
            | i == 0 = []
            | otherwise = i `mod` 2 : doSeq (i `div` 2)

validSeq :: [Int] -> Bool
validSeq xs
    | head xs == 1 = False
    | odd . length $ xs = False
    | otherwise = doValidate xs
    where
        doValidate [] = True
        doValidate (y:[]) = True
        doValidate (x:s@(y:_)) 
            | x /= y = doValidate s
            | otherwise = False

printList :: Show a => [a] -> IO ()
printList [] = return ()
printList (x:xs) = do
    print x
    printList xs

main = do
    contents <- readFile "input25.txt"
    let contents' = "cpy 2 a\ntgl a\ntgl a\ntgl a\ncpy 1 a\ndec a\ndec a"
        instructions' = lines contents
        instructions = A.listArray (0, (toInteger . length) instructions' - 1) instructions'

    printList $ execute registers instructions 0
    print $ head . filter (\(_, x) -> validSeq x) . zip [0,1..] . map seq $ [0,1..]
