
module Main where

import System.IO
import Data.List
import Data.Array (Array)
import qualified Data.Array as A
import Data.List.Split (splitOn)
import Queue (Queue)
import qualified Queue as Q
import Data.Set (Set)
import qualified Data.Set as S

main = do
    contents <- readFile "input12.txt"
    let input = lines $ contents     

    print . doPart1 $ input
    print . doPart2 $ input


doPart1 :: [String] -> Int
doPart1 input = 
    let pipes :: Array Int [Int]
        pipes = A.array (0, 1999) . map mapPipe $ input
    in
        S.size .
        findPool pipes (Q.singleton 0) $ S.empty

doPart2 :: [String] -> Int
doPart2 input =
    let pipes :: Array Int [Int]
        pipes = A.array (0, 1999) . map mapPipe $ input
    in
        numberOfGroups pipes [0..1999]

mapPipe :: String -> (Int, [Int])
mapPipe pipe = (id, programs)
    where
        id = read . takeWhile (/= ' ') $ pipe
        rest = tail . dropWhile (/= '>') $ pipe
        programs = map read . splitOn ", " $ rest

findPool :: Array Int [Int] -> Queue Int -> Set Int -> Set Int
findPool pipes queue set 
    | Q.null queue = set
    | otherwise = findPool pipes queue' set'
    where
        (x, q) = Q.pop queue

        (set', queue') =
            case S.member x set of
                True  -> ( set, q)
                False -> ( S.insert x set
                         , Q.pushList q $ pipes A.! x
                         )

numberOfGroups :: Array Int [Int] -> [Int] -> Int
numberOfGroups pipes [] = 0
numberOfGroups pipes xs@(x:_) = 
    1 + numberOfGroups pipes (filtered)
    where
        set = findPool pipes (Q.singleton x) $ S.empty
        filtered = filter (\a -> S.notMember a set) xs
