{-# LANGUAGE BangPatterns #-}
module Main where

import System.IO (readFile)
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Tuple (swap)
import Control.Applicative
import Data.Foldable (toList)

type State = (Map Int Int, Seq Int, Int)
type State' = ([(Int, Int)], Seq Int, Int)

main :: IO ()
main = do
    let players = 468
        marble = 71843

    -- print $ problem1 players (marble*10)
    -- print $ problem1 players (marble)
    print $ problem1' players (marble*100)
    -- print $ problem1 players (marble*100)
    -- mapM_ print $problem1 players marble
    --print $ problem2 players marble

problem1 players marble = highScore
    . (\(a,_,_) -> a)
    . (flip (!!)) (marble+1)
    . iterate (takeTurn players)
    $ (M.empty, Seq.singleton 0, 1)

problem1' players marble
    = highScore
    . (\(a,_,_) -> a)
    . play marble (takeTurn players)
    $ (M.empty, Seq.singleton 0, 1)

problem2 players marble = 
    let (scores, marbles, next) =
            (\(a,b,c) -> ((23, head $ M.elems a):[], b, c))
            . play 23 (takeTurn players)
            $ (M.empty, Seq.singleton 0, 1)
    in 
        (scores, marbles, next)


play :: Int -> (State -> State) -> State -> State
play !n f !state 
    | n == 0 = state
    | otherwise = play (n-1) f $ (f state)


-- Simulate 23 turns
subplay :: State' -> State'
subplay a@(score, marbles, value) =
    ((value+23, value+23+x):score, (second' Seq.>< rest) Seq.>< first', value+23)
    
    where
        marbles' = Seq.drop 1 marbles Seq.|> Seq.index marbles 0
        (used, rest) = Seq.splitAt 22 marbles'
        (first, x, second) =
            (\(a,b) -> (a, Seq.index b 0, Seq.drop 1 b)) . Seq.splitAt 18 $ used
        first' = Seq.fromList $ combine (toList first) [(value+1)..(value+18)]
        second' = Seq.fromList $ combine [(value+19)..(value+22)] (toList second)


combine :: [a] -> [a] -> [a]
combine [] _ = []
combine (x:xs) ys = x : combine ys xs

takeTurn :: Int -> State -> State
takeTurn maxPlayers (!score, !marbles, !value)
    | value `mod` 23 == 0 =
        ( M.insertWith (+) player (value + Seq.index marbles toRemove) $! score
        , uncurry (Seq.><) . swap . (\(a,b) -> (a, Seq.drop 1 b) ) . Seq.splitAt toRemove $! marbles
        , value + 1)
    | otherwise = (score, marbles', value+1)

    where
        nextInsert = 2 `mod` Seq.length marbles
        player = value `mod` maxPlayers
        marbles' = (Seq.<|) value . uncurry (Seq.><) . swap . Seq.splitAt nextInsert $! marbles
        toRemove = (Seq.length marbles) - 7

highScore :: Map Int Int -> Int
highScore = M.foldl (max) 0
