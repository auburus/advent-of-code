module Day08 where

import Data.Array (Array)
import qualified Data.Array as Array
import Data.List
import Data.List.Split (splitOn, wordsBy)
import Data.Maybe (Maybe)
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import System.IO

type Instruction = (String, Char, Int)

type Pointer = Int

type Acc = Integer

type State = (Pointer, Acc, Set Pointer)

main = do
  input <- fmap (map parseIns . lines) $ readFile "input08.txt"

  print $ problem1 input

  print $ problem2 input

problem1 input = run input (0, 0, Set.empty)

problem2 input = head . Maybe.mapMaybe (flip maybeRun (0, 0, Set.empty)) $ mutateInstructions input

parseIns :: String -> Instruction
parseIns str =
  let (op : value : []) = splitOn " " str
      sign = value !! 0
      num_value = read . drop 1 $ value
   in (op, sign, num_value)

run :: [Instruction] -> State -> Acc
run ins (ptr, acc, visited)
  | ptr `Set.member` visited = acc
  | otherwise = run ins $ step (ins !! ptr) (ptr, acc, visited)

run' :: [Instruction] -> State -> [State]
run' ins (ptr, acc, visited)
  | ptr `Set.member` visited = (ptr, acc, visited) : []
  | otherwise = (ptr, acc, visited) : (run' ins $ step (ins !! ptr) (ptr, acc, visited))

step :: Instruction -> State -> State
step ("nop", _, _) (ptr, acc, visited) = (ptr + 1, acc, ptr `Set.insert` visited)
step ("acc", '+', val) (ptr, acc, visited) = (ptr + 1, acc + toInteger val, ptr `Set.insert` visited)
step ("acc", '-', val) (ptr, acc, visited) = (ptr + 1, acc - toInteger val, ptr `Set.insert` visited)
step ("jmp", '+', val) (ptr, acc, visited) = (ptr + val, acc, ptr `Set.insert` visited)
step ("jmp", '-', val) (ptr, acc, visited) = (ptr - val, acc, ptr `Set.insert` visited)

maybeRun :: [Instruction] -> State -> Maybe Acc
maybeRun ins (ptr, acc, visited)
  | ptr >= length ins = Just acc
  | ptr `Set.member` visited = Nothing
  | otherwise = maybeRun ins $ step (ins !! ptr) (ptr, acc, visited)

mutateInstructions :: [Instruction] -> [[Instruction]]
mutateInstructions instructions =
  nub
    . take (length instructions)
    . map (\(i, ins) -> (take i ins) ++ [(mutateSingle (ins !! i))] ++ (drop (i + 1) ins))
    . zip [0, 1 ..]
    . repeat
    $ instructions

mutateSingle :: Instruction -> Instruction
mutateSingle ("jmp", a, b) = ("nop", a, b)
mutateSingle ("nop", a, b) = ("jmp", a, b)
mutateSingle x = x