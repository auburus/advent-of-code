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
    [input1, input2] <- splitOn "\n\n\n\n" <$> readFile "input16.txt"
    let input1' = map parseInput1 . splitOn "\n\n" $ input1
        input2' = map (map read . splitOn " ") . lines $ input2 :: [[Int]]

    print $ problem1 input1'
    -- mapM_ print $ helper input1'

    print $ problem2 input2'

problem2 :: [[Int]] -> Registers
problem2 input = run input $ M.fromList [(0,0), (1,0), (2,0), (3,0)]
    where
        run [] reg = reg
        run (x:xs) reg = run xs $ (ops !! (head x)) reg x

problem1 :: [(Registers, [Int], Registers)] -> Int
problem1 input = run input 0
    where
        run [] n = n
        run (x:xs) n 
            | (length . filter (match x) $ ops) >= 3 = run xs (n+1)
            | otherwise = run xs n

helper :: [(Registers, [Int], Registers)] -> [(String, [Int])]
helper input = 
    let inputByIns = map (\i -> filter (\(_, ins, _) -> head ins == i) input) [0..15]

    in 
        zip (map ((++) "OpCode " . show) [0..15])
        $ map (\input' -> filter (flip notElem found) . map fst . filter (\(_, op) -> all id $ map (flip match op) input') $ zip [0..15] ops) inputByIns


match :: (Registers, [Int], Registers) -> (Registers -> [Int] -> Registers) -> Bool
match (before, ins, after) op = (op before ins) == after

parseInput1 :: String -> (Registers, [Int], Registers)
parseInput1 str =
    let [l1, l2, l3] = lines str
        parse1 = \[_, _, x, _, y, _, z, _, t, _] -> map read [x,y,z,t]
        parse3 = \[_, _, _, x, _, y, _, z, _, t, _] -> map read [x,y,z,t]
        l1' = M.fromList . zip [0..3] . parse1 . splitOneOf "[], " $ l1
        l2' = map read . splitOn " " $ l2
        l3' = M.fromList . zip [0..3] . parse3 . splitOneOf "[], " $ l3
    in
        (l1', l2', l3')

ops :: [(Registers -> [Int] -> Registers)]
ops = [ bani, banr, muli, setr
      , bori, eqrr, gtir, mulr
      , gtrr, seti, gtri, eqri
      , addi, borr, eqir, addr
      ]

-- found = [11, 8, 10, 14, 5, 6, 1, 0, 3, 9, 4, 13, 15, 2, 7, 12]
found = []

correspondences :: Map Int (Registers -> [Int] -> Registers)
correspondences = M.fromList [(11, eqri), (8, gtrr), (10, gtri)]



addr :: Registers -> [Int] -> Registers
addr reg [opcode, a, b, c] =
    M.insert c (reg M.! a + reg M.! b) reg

addi :: Registers -> [Int] -> Registers
addi reg [opcode, a, b, c] =
    M.insert c (reg M.! a + b) reg

mulr :: Registers -> [Int] -> Registers
mulr reg [opcode, a, b, c] =
    M.insert c (reg M.! a * reg M.! b) reg

muli :: Registers -> [Int] -> Registers
muli reg [opcode, a, b, c] =
    M.insert c (reg M.! a * b) reg

banr :: Registers -> [Int] -> Registers
banr reg [opcode, a, b, c] =
    M.insert c (reg M.! a .&. reg M.! b) reg

bani :: Registers -> [Int] -> Registers
bani reg [opcode, a, b, c] =
    M.insert c (reg M.! a .&. b) reg

borr :: Registers -> [Int] -> Registers
borr reg [opcode, a, b, c] =
    M.insert c (reg M.! a .|. reg M.! b) reg

bori :: Registers -> [Int] -> Registers
bori reg [opcode, a, b, c] =
    M.insert c (reg M.! a .|. b) reg

setr :: Registers -> [Int] -> Registers
setr reg [opcode, a, b, c] =
    M.insert c (reg M.! a) reg

seti :: Registers -> [Int] -> Registers
seti reg [opcode, a, b, c] =
    M.insert c a reg

gtir :: Registers -> [Int] -> Registers
gtir reg [opcode, a, b, c] =
    M.insert c (if a > reg M.! b then 1 else 0) reg

gtri :: Registers -> [Int] -> Registers
gtri reg [opcode, a, b, c] =
    M.insert c (if (reg M.! a) > b then 1 else 0) reg

gtrr :: Registers -> [Int] -> Registers
gtrr reg [opcode, a, b, c] =
    M.insert c (if (reg M.! a) > (reg M.! b) then 1 else 0) reg

eqir :: Registers -> [Int] -> Registers
eqir reg [opcode, a, b, c] =
    M.insert c (if a == reg M.! b then 1 else 0) reg

eqri :: Registers -> [Int] -> Registers
eqri reg [opcode, a, b, c] =
    M.insert c (if (reg M.! a) == b then 1 else 0) reg

eqrr :: Registers -> [Int] -> Registers
eqrr reg [opcode, a, b, c] =
    M.insert c (if (reg M.! a) == (reg M.! b) then 1 else 0) reg

