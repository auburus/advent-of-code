module Main where

import System.IO (readFile)
import Data.List

import Data.Array (Array, Ix)
import qualified Data.Array as A

data Reindeer = Vixen
              | Blitzen
              | Rudolph
              | Cupid
              | Donner
              | Dasher
              | Comet
              | Prancer
              | Dancer
              deriving (Eq, Ord, Ix, Show)

data ReindeerState = Flying Int
                   | Resting Int
                   deriving (Eq, Show)

type Speed = Int
type Sec = Int
type RaceState = (Reindeer, ReindeerState, Int)
type Speeds = Array Reindeer (Speed, Sec, Sec)

step :: Speeds -> [RaceState] -> [RaceState]
step speeds = map (move speeds)

move :: Speeds -> RaceState -> RaceState
move speeds (reindeer, state, dist) = 
    case state of
        Flying i
            | i == 0 -> (reindeer, Resting (totalRest-1), dist)
            | otherwise -> (reindeer, Flying (i-1), dist + speed)
        Resting i
            | i == 0 -> (reindeer, Flying (totalFly-1), dist + speed)
            | otherwise -> (reindeer, Resting (i-1), dist)
    where
        (speed, totalFly, totalRest) = speeds A.! reindeer

parseInput :: String -> (Reindeer, (Speed, Sec, Sec))
parseInput str = (rein, (spd, totalFly, totalRest))
    where
        splitted = words str
        rein = parseReindeer . head $ splitted
        spd = read (splitted !! 3)
        totalFly = read (splitted !! 6)
        totalRest = read (splitted !! 13)

parseReindeer str
    | str == "Vixen" = Vixen
    | str == "Blitzen" = Blitzen
    | str == "Rudolph" = Rudolph
    | str == "Cupid" = Cupid
    | str == "Donner" = Donner
    | str == "Dasher" = Dasher
    | str == "Comet" = Comet
    | str == "Prancer" = Prancer
    | str == "Dancer" = Dancer

initial :: [RaceState]
initial = map (\x -> (x, Resting 0, 0)) reindeers
    where
        reindeers = [ Vixen, Blitzen, Rudolph, Cupid, Donner
                    , Dasher, Comet, Prancer, Dancer]

problem1 :: Speeds -> [RaceState] -> Int -> Int
problem1 speeds state i = maximum . map (\(_,_,a)->a) $ finalState
    where
        finalState = (iterate (step speeds) $ state) !! i

problem2 :: Speeds -> [RaceState] -> Int -> Int
problem2 speeds state i = maximum .
                          map (\x -> x - 1) . -- I am counting the 1st state where all tie
                          foldl (\total new -> zipWith (+) total new) initial'.
                          map points $ states
    where
        initial' = map (\_ -> 0) initial
        states = take i .
                 iterate (step speeds) $ state

points :: [RaceState] -> [Int]
points state = map (\(_,_,dist) -> if dist == max' then 1 else 0) state
    where
        max' = maximum . map (\(_,_,dist) -> dist) $ state

main = do
    contents <- readFile "input14.txt"
    let input = map parseInput . lines $ contents
        speeds = A.array (Vixen, Dancer) input

    print . problem1 speeds initial $ 2503
    print . problem2 speeds initial $ 2503
