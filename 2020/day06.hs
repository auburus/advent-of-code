module Day06 where

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
  input <- fmap (map lines . splitOn "\n\n") $ readFile "input06.txt"

  print $ problem1 input
  print $ problem2 input

problem1 = sum . map (length . nub . foldl (++) "")

problem2 = sum . map answersPerGroup

answersPerGroup :: [String] -> Int
answersPerGroup group = length . filter (groupHasAnswered group) $ "abcdefghijklmnopqrstuvwxyz"

groupHasAnswered :: [String] -> Char -> Bool
groupHasAnswered group letter = all (elem letter) group