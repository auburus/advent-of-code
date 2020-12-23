module Day09 where

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
  input <- fmap (map read . lines) $ readFile "input09.txt" :: IO [Int]

  print $ problem1 input
  let target = problem1 input
  print $ problem2 target input

problem1 input = findFirstInvalidXMAS (take 25 input) (drop 25 input)

problem2 target = weakness . subRangeThatAddsToN target

weakness :: [Int] -> Int
weakness xs = minimum xs + maximum xs

subRangeThatAddsToN :: Int -> [Int] -> [Int]
subRangeThatAddsToN n = head . filter ((== n) . sum) . inits . head . dropWhile (not . hasPrefixThatAddsToN n) . tails

hasPrefixThatAddsToN :: Int -> [Int] -> Bool
hasPrefixThatAddsToN n [] = False
hasPrefixThatAddsToN n (_ : []) = False
hasPrefixThatAddsToN n xs = run n xs 0
  where
    run _ [] _ = False
    run n (x : xs) sum
      | (sum + x) == n = True
      | (sum + x) > n = False
      | otherwise = run n xs (sum + x)

findFirstInvalidXMAS :: [Int] -> [Int] -> Int
findFirstInvalidXMAS preamble (x : xs)
  | hasPairThatAddsToN x preamble = findFirstInvalidXMAS ((drop 1 preamble) ++ [x]) xs
  | otherwise = x

hasPairThatAddsToN :: Int -> [Int] -> Bool
hasPairThatAddsToN target values =
  (> 1)
    . length
    . filter (flip elem values . (-) target)
    $ values