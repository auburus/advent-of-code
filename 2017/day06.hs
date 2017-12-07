module Main where

import System.IO
import Data.List
import Data.Array (Array)
import qualified Data.Array as A
import Data.Set (Set)
import qualified Data.Set as S

import Data.Map (Map)
import qualified Data.Map as M


main = do
    let input = A.listArray (0, 15)
            [11, 11, 13, 7, 0, 15, 5, 5, 4, 4, 1, 1, 7, 1, 15, 11]
        input' = A.listArray (0, 3) [0, 2, 7, 0]


    print . doPart1 $ input
    print . doPart2 $ input

doPart1 :: Array Int Int -> Int
doPart1 input = countCycles input

doPart2 :: Array Int Int -> Int
doPart2 input = countCycles2 input

maxIndex :: Array Int Int -> Int
maxIndex blocks = 
    maxIndex' (head blocks') (tail blocks')
    where
        blocks' = A.assocs blocks
        maxIndex' (maxI, maxE) [] = maxI
        maxIndex' (maxI, maxE) ((i,e):xs)
            | e > maxE = maxIndex' (i, e) xs
            | otherwise = maxIndex' (maxI, maxE) xs

putInBins :: Int -> Int -> [Int]
putInBins nBins i = 
    take nBins . putReminding list' $ (i `mod` nBins)
    where
        list' = repeat (i `div` nBins)
        putReminding (x:xs) i
            | i == 0 = (x:xs)
            | otherwise = (x+1) : putReminding xs (i-1)

reallocate :: Array Int Int -> Array Int Int
reallocate blocks =
    let maxIx = maxIndex blocks
        len = length . A.indices $ blocks
        bins = putInBins len (blocks A.! maxIx)
        blocks' = blocks A.// [(maxIx, 0)]
        listBlocks = take len . drop (maxIx + 1) . cycle . A.assocs $ blocks'
        reallocated = zipWith (\(i, e) bin -> (i, e + bin)) listBlocks bins

    in A.array (A.bounds blocks) reallocated

countCycles :: Array Int Int -> Int
countCycles blocks = countCycles' S.empty blocks 0

countCycles2 :: Array Int Int -> Int
countCycles2 blocks = countCycles'' M.empty blocks 0

countCycles' :: Set (Array Int Int) -> Array Int Int -> Int -> Int
countCycles' set blocks moves
    | S.member blocks set = moves
    | otherwise = countCycles' updatedSet (reallocate blocks) (moves + 1)

    where
        updatedSet = S.insert blocks set

countCycles'' :: Map (Array Int Int) Int -> Array Int Int -> Int -> Int
countCycles'' map blocks moves
    | M.member blocks map = moves - (map M.! blocks)
    | otherwise = countCycles'' updatedMap (reallocate blocks) (moves + 1)

    where
        updatedMap = M.insert blocks moves map
