module Main where

import System.IO
import qualified Data.Array as A
import qualified Data.Map.Strict as Map


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

toggle :: String -> String
toggle instruction 
    | nArgs == 1 && operation == "inc" =
        unwords $ "dec" : tail splitted
    | nArgs == 1 =
        unwords $ "inc" : tail splitted
    | nArgs == 2 && operation == "jnz" =
        unwords $ "cpy" : tail splitted
    | nArgs == 2 =
        unwords $ "jnz" : tail splitted

    where
        splitted = words instruction
        nArgs = length splitted - 1
        operation = head splitted

updateInstructions :: Map.Map String Integer -> A.Array Integer String -> Integer -> A.Array Integer String
updateInstructions registers instructions i
    | operation == "tgl" && validInstruction instructions (i + toggleArg)
        = instructions A.// [(i + toggleArg, toggle toggleIns)] 
    | otherwise = instructions

    where
        current = instructions A.! i
        splitted = words current
        operation = head splitted
        toggleArg = getValue registers (splitted !! 1)
        toggleIns = instructions A.! (i + toggleArg)

-- Special case. update registers accordingly
loop1 :: Map.Map String Integer -> Map.Map String Integer
loop1 = zero "d" . zero "c" . incrementA
    where
        incrementA reg = Map.adjust (\x -> x + (reg Map.! "b" * reg Map.! "d")) "a" reg
        zero key = Map.adjust (\_ -> 0) key

execute :: Map.Map String Integer -> A.Array Integer String -> Integer -> Map.Map String Integer
execute registers instructions i
    | i == 4 = execute (loop1 registers) instructions 9
    | not $ validInstruction instructions i = registers
    | otherwise =
        execute registers' instructions' (nextInstruction registers current i)

    where
        current = instructions A.! i
        registers' = updateRegisters registers current
        instructions' = updateInstructions registers instructions i

registers1 = Map.fromList [("a", 7), ("b", 0), ("c", 0), ("d", 0)]
registers2 = Map.fromList [("a", 12), ("b", 0), ("c", 0), ("d", 0)] 


printList :: Show a => [a] -> IO ()
printList [] = return ()
printList (x:xs) = do
    print x
    printList xs

main = do
    contents <- readFile "input23.txt"
    let contents' = "cpy 2 a\ntgl a\ntgl a\ntgl a\ncpy 1 a\ndec a\ndec a"
        instructions' = lines contents
        instructions = A.listArray (0, (toInteger . length) instructions' - 1) instructions'

    print $ execute registers1 instructions 0
    print $ execute registers2 instructions 0
