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

type Turn = Int

type Play = (Turn, Int)

type State = (Play, Map Int Int)

main = do
  let input = [1, 20, 11, 6, 12, 0] :: [Int]

  print $ problem1 input
  print $ problem2 input

problem1 = snd . playOnTurn 2020

problem2 = snd . playOnTurn 30000000

playOnTurn :: Int -> [Int] -> Play
playOnTurn turn xs = fst . head . dropWhile ((/= turn) . fst . fst) $ iterate gameTurn (lastPlay, initialGame)
  where
    initialPlays = zip [1 ..] $ xs
    initialGame = Map.fromList . init . map (\(a, b) -> (b, a)) $ initialPlays
    lastPlay = last initialPlays

gameTurn :: State -> State
gameTurn (lastplay@(turn, x), game) =
  case Map.lookup x game of
    Nothing -> ((turn + 1, 0), game')
    Just turn' -> ((turn + 1, turn - turn'), game')
  where
    game' = Map.insert x turn game
