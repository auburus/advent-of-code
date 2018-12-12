{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Char
import System.IO (readFile)
import Data.List
import Data.List.Split (splitOneOf)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.Array (Array)
import qualified Data.Array as A
import Control.Applicative

main :: IO ()
main = do
    let initial = zip [0,1..] "####....#...######.###.#...##....#.###.#.###.......###.##..##........##..#.#.#..##.##...####.#..##.#"
    input <- foldl (\m x -> M.insert (fst x) (head . snd $ x) m) M.empty
            . map parseInput
            . lines
            <$> readFile "input12.txt"

    print $ problem1 initial input
    print $ problem2 initial input


problem1 initial trans = 
    let final = generations 100 trans initial
    in
        sum . map fst . filter ((=='#') . snd) $ final

problem2 initial trans =
    let (start, period) = findCycle 0 trans S.empty initial
        incr = score (generations (start + period) trans initial) 
             - score (generations start trans initial)
    in
        score (generations start trans initial)
        + ((50000000000 - start) `div` period) * incr


score :: [(Int, Char)] -> Int
score = sum . map fst . filter ((=='#') . snd)

generations :: Int -> Map String Char -> [(Int, Char)] -> [(Int, Char)]
generations 0 _ state = state
generations !n trans !state = generations (n-1) trans (spread trans . addEmpty $ state)

findCycle :: Int -> Map String Char -> Set String -> [(Int, Char)] -> (Int, Int)
findCycle n trans seen xs
    | current `S.member` seen = (n, realCycle 0 S.empty $ xs)
    | otherwise =
        findCycle (n+1) trans (S.insert current seen) (spread trans . addEmpty $ xs)

    where
        current = map snd xs

        realCycle i seen' xs'
            | current' `S.member` seen' = i
            | otherwise =
                realCycle (i+1) (S.insert current' seen') (spread trans . addEmpty $ xs')

            where
                current' = map snd xs'

parseInput :: String -> (String, String)
parseInput input = mymap . splitOneOf " " $ input
    where
        mymap [p, _, o] = (p, o)

addEmpty xs = h ++ clear ++ t 
    where
        idx_h = fst . head $ clear
        idx_t = fst . last $ clear
        h = map (\a -> (a,'.')) $ [(idx_h - 4)..(idx_h-1)]
        t = map (\a -> (a,'.')) $ [(idx_t+1)..(idx_t+4)]
        clear = trim . trim $ xs
        trim = reverse . dropWhile ((=='.') . snd)


spread :: Map String Char -> [(Int, Char)] -> [(Int, Char)]
spread trans xs
    | length xs < 5 = []
    | otherwise = (fst $ xs !! 2, trans M.! (map snd . take 5 $ xs)) : spread trans (tail xs)
