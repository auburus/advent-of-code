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

type Pos = (Int, Int, Int, Int)

readInput :: String -> IO ([String])
readInput f = fmap lines $ readFile f

main = do
  input <- readInput "input17.txt"

  print $ problem1 input
  print $ problem2 input

problem1 input = Set.size . head . drop 6 . iterate step . activeCells $ input

problem2 input = Set.size . head . drop 6 . iterate step' . activeCells $ input

activeCells :: [String] -> Set Pos
activeCells input = Set.fromList . foldl (++) [] . map (\(xs, y) -> map (\x -> (x, y, 0, 0)) xs) $ zip (map activeCellPositions input) [0, 1 ..]

activeCellPositions :: String -> [Int]
activeCellPositions str = map fst . filter ((== '#') . snd) $ zip [0, 1 ..] str

neighbours :: Pos -> Set Pos
neighbours (x, y, z, w) = Set.fromList [(x + i, y + j, z + k, w) | i <- [-1, 0, 1], j <- [-1, 0, 1], k <- [-1, 0, 1], (i, j, k) /= (0, 0, 0)]

neighbours' :: Pos -> Set Pos
neighbours' (x, y, z, w) = Set.fromList [(x + i, y + j, z + k, w + l) | i <- [-1, 0, 1], j <- [-1, 0, 1], k <- [-1, 0, 1], l <- [-1, 0, 1], (i, j, k, l) /= (0, 0, 0, 0)]

step :: Set Pos -> Set Pos
step active =
  let candidates = Set.foldl Set.union active $ Set.map neighbours active
   in Set.filter (goesOrStaysActive active) candidates

goesOrStaysActive :: Set Pos -> Pos -> Bool
goesOrStaysActive active pos
  | pos `Set.member` active && (Set.size active_neighbours == 2 || Set.size active_neighbours == 3) = True
  | pos `Set.notMember` active && (Set.size active_neighbours == 3) = True
  | otherwise = False
  where
    active_neighbours = active `Set.intersection` (neighbours pos)

step' :: Set Pos -> Set Pos
step' active =
  let candidates = Set.foldl Set.union active $ Set.map neighbours' active
   in Set.filter (goesOrStaysActive' active) candidates

goesOrStaysActive' :: Set Pos -> Pos -> Bool
goesOrStaysActive' active pos
  | pos `Set.member` active && (Set.size active_neighbours == 2 || Set.size active_neighbours == 3) = True
  | pos `Set.notMember` active && (Set.size active_neighbours == 3) = True
  | otherwise = False
  where
    active_neighbours = active `Set.intersection` (neighbours' pos)