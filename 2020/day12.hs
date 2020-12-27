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

type Pos = (Int, Int)

type Dir = Char

type State = (Pos, Dir)

type Instruction = (Char, Int)

readInput :: String -> IO [Instruction]
readInput = fmap (map (\(x : xs) -> (x, read xs)) . lines) . readFile

main = do
  input <- readInput "input12.txt"

  print $ problem1 input
  print $ problem2 input

problem1 = distance . foldl move ((0, 0), 'E')

problem2 = distance' . foldl move' ((0, 0), (10, -1))

distance :: State -> Int
distance ((x, y), _) = (abs x) + (abs y)

distance' :: State' -> Int
distance' ((x, y), _) = (abs x) + (abs y)

move :: State -> Instruction -> State
move state (_, 0) = state
move ((x, y), dir) ('N', val) = ((x, y - val), dir)
move ((x, y), dir) ('S', val) = ((x, y + val), dir)
move ((x, y), dir) ('E', val) = ((x + val, y), dir)
move ((x, y), dir) ('W', val) = ((x - val, y), dir)
move ((x, y), dir) ('L', val) =
  case dir of
    'N' -> move ((x, y), 'W') ('L', val -90)
    'W' -> move ((x, y), 'S') ('L', val -90)
    'S' -> move ((x, y), 'E') ('L', val -90)
    'E' -> move ((x, y), 'N') ('L', val -90)
move state ('R', val) = move state ('L', 360 - val)
move ((x, y), dir) ('F', val) = move ((x, y), dir) (dir, val)

type State' = (Pos, Pos)

move' :: State' -> Instruction -> State'
move' state (_, 0) = state
move' (ship, (wx, wy)) ('N', val) = (ship, (wx, wy - val))
move' (ship, (wx, wy)) ('S', val) = (ship, (wx, wy + val))
move' (ship, (wx, wy)) ('E', val) = (ship, (wx + val, wy))
move' (ship, (wx, wy)) ('W', val) = (ship, (wx - val, wy))
move' (ship, (wx, wy)) ('L', val) = move' (ship, (wy, - wx)) ('L', val -90)
move' state ('R', val) = move' state ('L', 360 - val)
move' ((sx, sy), (wx, wy)) ('F', val) = ((sx + val * wx, sy + val * wy), (wx, wy))
