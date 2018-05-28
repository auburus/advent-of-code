module Main where

import System.IO (readFile)
import Data.List
import Text.Read

import Data.Map (Map)
import qualified Data.Map as M

data Param = Register Char | Value Int deriving (Show, Eq)
data Instruction = Set Param Param
                 | Sub Param Param
                 | Mul Param Param
                 | Jnz Param Param
                 deriving (Show, Eq)

type Registers = Map Char Int

main = do
    contents <- readFile "input23.txt"
    let input = map (parseLine . words) . lines $ contents
        firstB = 81 * 100 + 100000
        len = 1000

    --mapM_ print . take 10 . doPart1 $ input
    print . doPart1 $ input

    print $ doPart2 firstB len

registers :: Registers
registers = M.insert 'a' 0
          . M.insert 'b' 0
          . M.insert 'c' 0
          . M.insert 'd' 0
          . M.insert 'e' 0
          . M.insert 'f' 0
          . M.insert 'g' 0
          . M.insert 'h' 0
          $ M.empty

doPart1 :: [Instruction] -> Int
doPart1 instructions = run registers instructions 0

parseLine :: [String] -> Instruction
parseLine (ins:p1:p2:_)
    | ins == "set" = Set p1' p2'
    | ins == "sub" = Sub p1' p2'
    | ins == "mul" = Mul p1' p2'
    | ins == "jnz" = Jnz p1' p2'
    where p1' = parseParam p1
          p2' = parseParam p2

parseParam :: String -> Param
parseParam s@(c:_)
    | c `elem` "abcdefgh" = Register c
    | otherwise = Value (read s)

run :: Registers -> [Instruction] -> Int -> Int
run reg instructions i
    | i < 0 || i >= length instructions = 0
    | otherwise = 
        let ins = instructions !! i
            (reg', i') = runInstruction reg ins
        in case ins of
            Mul _ _ -> 1 + run reg' instructions (i + i')
            _ -> run reg' instructions (i + i')

runInstruction :: Registers -> Instruction -> (Registers, Int)
runInstruction reg ins = 
    case ins of
        Set (Register r) y ->
            (M.insert r (getValue reg y) reg, 1)
        Sub (Register r) y ->
            (M.insert r (getValue reg (Register r) - getValue reg y) reg, 1)
        Mul (Register r) y ->
            (M.insert r (getValue reg (Register r) * getValue reg y) reg, 1)
        Jnz x y -> (reg, if getValue reg x /= 0 then getValue reg y else 1)


getValue :: Registers -> Param -> Int
getValue registers (Register r) = registers M.! r
getValue registers (Value a) = a


-- In fact, we want to know the amount of prime numbers between 108300 and 125300
-- that correspond to the form (108300 + 17*i)
doPart2 :: Int -> Int -> Int
doPart2 init len = 
    let bs = take (len+1) [ init + 17*i | i <- [0,1..] ]
        p = primes (init + 17 * len)
    in
        length $ filter (\x -> x `notElem` p) bs
        

primes :: Int -> [Int]
primes max 
    | max <= 2 = [max]
    | otherwise = sieve [2..max]
    where
        sieve [] = []
        sieve (x:xs) = x : sieve (filter (\a -> (a `mod` x) /= 0) xs)
        
{-


b = 83
c = b
if a != 0:
    b = b * 100
    b = b + 100000
    c = b
    c = c + 17000

while(1): // es fa 1000 vegades
    f = 1
    d = 2

    do 
        e = 2
        do
            if d * e == b:
                f = 0

            e = e + 1
        while (e != b)

        d++
    while (d != b)

    if ( f == 0)
        h = h + 1

    if b == c:
        exit()

    b = b + 17
end

-}
