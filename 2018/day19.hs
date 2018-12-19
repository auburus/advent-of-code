module Main where

import System.IO (readFile)
import Control.Applicative
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.List.Split (splitOn, splitOneOf)
import Data.Bits

type Registers = Map Int Int

main = do
    input <- map parseInput . tail . lines<$> readFile "input19.txt"
    ip <- read . last . splitOn " " . head . lines <$> readFile "input19.txt" :: IO Int


    print $ problem1 input ip

    putStrLn "After observing the code for a while, I realized that is the sum of divisors of 10996992:"

    print . sum $ divisors 10996992


parseInput :: String -> (Registers -> Registers)
parseInput str =
    let (name:params) = splitOn " " str
        params' = map read params :: [Int]
        toFunc "addr" = addr
        toFunc "addi" = addi
        toFunc "mulr" = mulr
        toFunc "muli" = muli
        toFunc "banr" = banr
        toFunc "bani" = bani
        toFunc "borr" = borr
        toFunc "bori" = bori
        toFunc "setr" = setr
        toFunc "seti" = seti
        toFunc "gtir" = gtir
        toFunc "gtri" = gtri
        toFunc "gtrr" = gtrr
        toFunc "eqir" = eqir
        toFunc "eqri" = eqri
        toFunc "eqrr" = eqrr
    in
        flip (toFunc name) params'


problem1 :: [(Registers -> Registers)] -> Int -> [(Int, Registers)]
problem1 ins ip = run ins ip . M.adjust (flip (-) 1) ip . M.fromList $ zip [0..5] [0,0..]

problem2 :: [(Registers -> Registers)] -> Int -> [(Int, Registers)]
problem2 ins ip =
    run ins ip . M.adjust (flip (-) 1) ip . M.fromList $ zip [0..5] [1,0,0,0,0,0]


run :: [(Registers -> Registers)] -> Int -> Registers -> [(Int, Registers)]
run instructions ip reg
    -- | i == 12 = [(i, reg')]
    | i == 3 && reg' M.! 1 == 2 =
        run instructions ip $ M.insert 1 10551376 reg
    | i < 0 || i >= length instructions = [(i, reg')]
    | otherwise = (run instructions ip $ (instructions !! i) reg')
    where
        reg' = M.adjust (+1) ip reg
        i = reg' M.! ip

addr :: Registers -> [Int] -> Registers
addr reg [a, b, c] =
    M.insert c (reg M.! a + reg M.! b) reg

addi :: Registers -> [Int] -> Registers
addi reg [a, b, c] =
    M.insert c (reg M.! a + b) reg

mulr :: Registers -> [Int] -> Registers
mulr reg [a, b, c] =
    M.insert c (reg M.! a * reg M.! b) reg

muli :: Registers -> [Int] -> Registers
muli reg [a, b, c] =
    M.insert c (reg M.! a * b) reg

banr :: Registers -> [Int] -> Registers
banr reg [a, b, c] =
    M.insert c (reg M.! a .&. reg M.! b) reg

bani :: Registers -> [Int] -> Registers
bani reg [a, b, c] =
    M.insert c (reg M.! a .&. b) reg

borr :: Registers -> [Int] -> Registers
borr reg [a, b, c] =
    M.insert c (reg M.! a .|. reg M.! b) reg

bori :: Registers -> [Int] -> Registers
bori reg [a, b, c] =
    M.insert c (reg M.! a .|. b) reg

setr :: Registers -> [Int] -> Registers
setr reg [a, b, c] =
    M.insert c (reg M.! a) reg

seti :: Registers -> [Int] -> Registers
seti reg [a, b, c] =
    M.insert c a reg

gtir :: Registers -> [Int] -> Registers
gtir reg [a, b, c] =
    M.insert c (if a > reg M.! b then 1 else 0) reg

gtri :: Registers -> [Int] -> Registers
gtri reg [a, b, c] =
    M.insert c (if (reg M.! a) > b then 1 else 0) reg

gtrr :: Registers -> [Int] -> Registers
gtrr reg [a, b, c] =
    M.insert c (if (reg M.! a) > (reg M.! b) then 1 else 0) reg

eqir :: Registers -> [Int] -> Registers
eqir reg [a, b, c] =
    M.insert c (if a == reg M.! b then 1 else 0) reg

eqri :: Registers -> [Int] -> Registers
eqri reg [a, b, c] =
    M.insert c (if (reg M.! a) == b then 1 else 0) reg

eqrr :: Registers -> [Int] -> Registers
eqrr reg [a, b, c] =
    M.insert c (if (reg M.! a) == (reg M.! b) then 1 else 0) reg


divisors :: Int -> [Int]
divisors a = filter ((==0) . (mod) a) [1..a]
