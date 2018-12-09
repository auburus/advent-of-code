module Main where

import Data.Char
import System.IO (readFile)
import Data.Text (split, splitOn, pack, unpack)
import Data.List
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.Array (Array)
import qualified Data.Array as A
import Control.Applicative

main :: IO ()
main = do
    input <- map parseInput . lines <$> readFile "input07.txt"
    let basic = M.fromList
              . map (\x -> (x, []))
              . nub . foldl (++) []
              . map (\(x,y) -> [x,y])
              $ input :: Map Char [Char]
        dep = foldl (\map (key, req) -> M.insertWith (++) key [req] map) basic $ input

    print $ complete dep 'C'
    print $ problem1 dep
    print $ problem2 dep

problem1 dep
    | M.null dep = []
    | otherwise = next dep : problem1 (complete dep (next dep))

problem2 dep = totalTime dep M.empty 5 0

parseInput input = mymap . map unpack . split ((flip elem) " ") . pack $ input
    where
        mymap [_, x, _, _, _, _, _, y, _, _] = (head y, head x)

next :: Map Char [Char] -> Char
next = minimum . possible

complete :: Map Char [Char] -> Char -> Map Char [Char]
complete map c = M.delete c $ M.map (delete c) map

time :: Char -> Int
time c = ord c - 4

possible :: Map Char [Char] -> [Char]
possible = map fst . M.toList . M.filter (null) 


totalTime :: Map Char [Char] -> Map Char Int -> Int -> Int -> Int
totalTime dep timeLeft workers t
    | M.null dep = t
    | not . null $ finished =
        totalTime (foldl (complete) dep finished)
                  (foldl (flip M.delete) timeLeft finished)
                  (workers + (length finished))
                  t
    | (not . null $ possible') && workers > 0 =
        totalTime dep (M.insert n' (time n') timeLeft) (workers - 1) t
    | otherwise = 
        totalTime dep (M.map ((flip (-)) waitTime) timeLeft) workers (t+waitTime)
    where
        possible' = filter (flip M.notMember $ timeLeft) $ possible dep
        n' = head possible'
        finished = map fst . M.toList . M.filter (==0) $ timeLeft
        waitTime = M.fold (min) 200 $ timeLeft

