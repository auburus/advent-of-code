module Main where

import Data.Array (Array)
import qualified Data.Array as Array
import Data.List
import Data.List.Split (splitOn, wordsBy)
import Data.Maybe (Maybe)
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import System.IO

type Offset = Integer

readInput :: String -> IO (Int, [Maybe Int])
-- readInput = fmap (\(id : l) -> (read id, (\x -> if x == "x" then Nothing else Just (read x)) $ splitOn "," l) . lines) . readFile
readInput = fmap ((\(id : l : []) -> (read id, map (\x -> if x == "x" then Nothing else Just (read x)) $ splitOn "," l)) . lines) . readFile

main = do
  input <- readInput "input13.txt"

  print $ problem1 input
  print $ offsetTimes (snd input)

offsetTimes :: [Maybe Int] -> [(Offset, Integer)]
offsetTimes a = map (\(a, Just b) -> (a, toInteger b)) . filter (Maybe.isJust . snd) $ zip [0, 1 ..] a

problem1 (start, multiples) =
  let (depart_time, multiple) = findEarliest start (Maybe.catMaybes multiples)
   in (depart_time - start) * multiple

findEarliest :: Int -> [Int] -> (Int, Int)
findEarliest start = minimumBy (\(x, _) (y, _) -> compare x y) . map (findEarliestMultiple start)

findEarliestMultiple :: Int -> Int -> (Int, Int)
findEarliestMultiple start multiple = (head . dropWhile (\n -> n `mod` multiple /= 0) $ [start, start + 1 ..], multiple)

isInteger :: (RealFrac a) => a -> Bool
isInteger x = abs (fromIntegral (round x) - x) < 1e-6

intersect' :: (Eq a, Ord a) => [[a]] -> [a]
intersect' (x : xs) = foldl intersectSorted x xs

intersectSorted :: (Eq a, Ord a) => [a] -> [a] -> [a]
intersectSorted (x : xs) (y : ys)
  | x == y = x : intersectSorted xs ys
  | x < y = intersectSorted xs (y : ys)
  | otherwise = intersectSorted (x : xs) ys