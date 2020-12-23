module Day10 where

import Data.Array (Array)
import qualified Data.Array as Array
import Data.List
import Data.List.Split (splitOn, wordsBy)
import Data.Maybe (Maybe)
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import System.IO

main = do
  input <- fmap (map read . lines) $ readFile "input10.txt" :: IO [Int]
  let input' = [0] ++ sort input ++ [maximum input + 3]

  print $ problem1 input'
  print $ problem2 input'

problem1 input = (length $ filter (== 1) adapter_deltas) * (length $ filter (== 3) adapter_deltas)
  where
    adapter_deltas = get_deltas input

problem2 = product . map (possible_combinations . length) . splitOn [3] . get_deltas

get_deltas :: [Int] -> [Int]
get_deltas [] = []
get_deltas (x : []) = []
get_deltas (x : y : xs) = (y - x) : get_deltas (y : xs)

possible_combinations :: Int -> Int
-- 5 8 11 --> 333
possible_combinations 0 = 1 -- 2^0
-- 5 8 9 12 --> 313
possible_combinations 1 = 1 -- 2^0
-- 5 8 9 10 13 --> 3113
-- 5 8 10 13 --> 323
possible_combinations 2 = 2 -- 2^1
-- 5 8 9 10 11 14 --> 31113
-- 5 8 10 11 14 --> 3213
-- 5 8 9 11 14 --> 3123
-- 5 8 11 14 --> 333
possible_combinations 3 = 4 -- 2^2
-- 5 8 9 10 11 12 15 --> 311113
-- 5 8 10 11 12 15 --> 32113
-- 5 8 9 11 12 15 --> 31213
-- 5 8 9 10 12 15 --> 31123
-- 5 8 10 12 15 --> 3223
-- 5 8 11 12 15 --> 3313
-- 5 8 9 12 15 --> 3133
possible_combinations 4 = 7 -- 2^3 - 1