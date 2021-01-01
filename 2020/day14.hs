module Main where

import Data.Array (Array)
import qualified Data.Array as Array
import Data.List
import Data.List.Split (splitOn, wordsBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (Maybe)
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import System.IO

type Mask = String

data Instruction = Mask Mask | Assign (Integer, Integer) deriving (Eq, Show)

isMask :: Instruction -> Bool
isMask (Mask _) = True
isMask _ = False

type Memory = Map Integer Integer

type State = (Mask, Memory)

readInput :: String -> IO [Instruction]
readInput = fmap (map parseInstruction . lines) . readFile

parseInstruction :: String -> Instruction
parseInstruction str
  | "mask" `isPrefixOf` str = Mask . dropWhile (flip elem " =") . drop 4 $ str
  | otherwise = Assign (addr, value)
  where
    addr = read . takeWhile (/= ']') . drop 4 $ str
    value = read . drop 2 . dropWhile (/= '=') $ str

main = do
  input <- readInput "input14.txt"

  print $ problem1 input
  print $ problem2 input

problem1 = Map.foldl (+) 0 . snd . foldl step ("", Map.empty)

problem2 = Map.foldl (+) 0 . snd . foldl step' ("", Map.empty)

applyMask :: Mask -> Integer -> Integer
applyMask [] value = value
applyMask mask value = binToDec $ zipWith op mask value'
  where
    value' = reverse . take 36 $ (reverse $ decToBin value) ++ repeat 0

op :: Char -> Int -> Int
op 'X' val = val
op a _ = read [a]

decToBin :: Integer -> [Int]
decToBin 0 = []
decToBin x = decToBin (x `div` 2) ++ [fromInteger $ x `mod` 2]

binToDec :: [Int] -> Integer
binToDec = snd . foldr (\x (idx, total) -> (idx + 1, (toInteger x) * round (2 ** idx) + total)) (0, 0)

step :: State -> Instruction -> State
step (_, memory) (Mask mask) = (mask, memory)
step (mask, memory) (Assign (addr, value)) = (mask, memory')
  where
    memory' = Map.insert addr (applyMask mask value) memory

-- type MultiAssign = (Address, Mask, Value)

-- maskAddr :: Mask -> Integer -> [Integer]
-- maskAddr mask addr =

applyMask' :: Mask -> Integer -> [Integer]
applyMask' mask addr = map binToDec . unfloat $ zipWith op' mask addr'
  where
    addr' = reverse . take 36 $ (reverse $ decToBin addr) ++ repeat 0

unfloat :: String -> [[Int]]
unfloat [] = [[]]
unfloat ('X' : xs) = unfloat ('0' : xs) ++ unfloat ('1' : xs)
unfloat (x : xs) = map ((:) (read [x])) $ unfloat xs

op' :: Char -> Int -> Char
op' '0' a = head . show $ a
op' a _ = a

step' :: State -> Instruction -> State
step' (_, memory) (Mask mask) = (mask, memory)
step' (mask, memory) (Assign (addr, value)) = (mask, memory')
  where
    memory' = foldl (\mem address -> Map.insert address value mem) memory $ applyMask' mask addr