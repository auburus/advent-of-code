module Day01 where

import Data.Array (Array)
import qualified Data.Array as Array
import Data.List.Split (wordsBy)
import Data.Maybe (Maybe)
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import System.IO

data Cell = Space | Tree deriving (Eq, Show)

type Slope = (Int, Int)

toCell c
  | c == '.' = Space
  | c == '#' = Tree

main = do
  input <- fmap (map (map toCell) . lines) $ readFile "input03.txt" :: IO [[Cell]]

  print $ problem1 input

  print $ problem2 input

problem1 = trees . move (1, 2) 0

problem2 input = foldl (*) 1 . map (\x -> trees . move x 0 $ input) $ [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

trees = length . filter (== Tree)

move :: Slope -> Int -> [[Cell]] -> [Cell]
move _ _ [] = []
move (_, 2) _ (_ : []) = []
move (sx, 1) x (line : forest) = (line !! x) : move (sx, 1) ((x + sx) `mod` (length line)) forest
move (sx, 2) x (line : _ : forest) = (line !! x) : move (sx, 2) ((x + sx) `mod` (length line)) forest
