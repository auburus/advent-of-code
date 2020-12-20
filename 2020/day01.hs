module Day01 where

import Data.Maybe (Maybe)
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import System.IO

main = do
  input <- fmap (map read . lines) $ readFile "input01.txt" :: IO [Int]

  print $ problem1 input
  print $ problem2 input

problem1 = foldl (*) 1 . pairThatAddsToN 2020

problem2 = foldl (*) 1 . tripletThatAddsToN 2020

pairThatAddsToN :: Int -> [Int] -> [Int]
pairThatAddsToN target values =
  filter
    (flip Set.member values' . (-) target)
    values
  where
    values' = Set.fromList values

tripletThatAddsToN :: Int -> [Int] -> [Int]
tripletThatAddsToN target values =
  Maybe.catMaybes $ map (\x -> if not . null $ pairThatAddsToN (target - x) values then Just x else Nothing) values