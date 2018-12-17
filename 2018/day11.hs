{-# LANGUAGE BangPatterns #-}

module Main where

import Data.List
import System.IO (readFile)
import Control.Applicative
import Data.Array.IArray (Array)
import qualified Data.Array.IArray as A
import Data.Map (Map)
import qualified Data.Map as M

main = do
    let input = 7857
        sumA = sumArray . grid $ input

    print $ problem1 sumA
    print $ problem2 sumA


problem1 :: Array (Int, Int) Int -> (Int, (Int, Int))
problem1 sumA =
    maximum . zip (map (sumValues sumA (3,3)) idx) $ idx
    where
        idx = [(i,j) | i <- [0..298], j <- [0..298]]

problem2 :: Array (Int, Int) Int -> (Int, (Int, Int, Int))
problem2 sumA =
    maximum . map (flip maxWithSize sumA . (\a -> (a,a))) $ [1..300]
    

maxWithSize :: (Int, Int) -> Array (Int, Int) Int -> (Int, (Int, Int, Int))
maxWithSize (x,y) sumA =
    maximum . zip (map (sumValues sumA (x,y)) idx) $ map (\(a,b) -> (a,b,x)) idx
    where
        idx = [ (i,j) | i <- [0..(301-x)], j <- [0..(301-y)]]


powerLevel :: Int -> (Int, Int) -> Int
powerLevel serial (x,y) =
    let id = x + 10
        hundrets = flip mod 10 . flip div 100
        
    in
        flip (-) 5 . hundrets $ (id * y + serial) * id


grid :: Int -> Array (Int, Int) Int
grid serial = A.array ((0,0), (300,300))
    [((i,j), powerLevel serial (i,j)) | i <- [0..300], j <- [0..300]]

sumArray :: Array (Int, Int) Int -> Array (Int, Int) Int
sumArray arr =
            A.array ((-1, -1), (300,300))
            . M.toList
            . run (0,0) arr
            $ M.union (M.fromList [((-1, x), 0) | x <- [-1..300]])
            (M.fromList [((x, -1),0) | x <- [0..300]])
    where
        run (i,j) arr sumA
            | j > maxJ = sumA
            | i > maxI = run (0, j+1) arr sumA
            | otherwise = run (i+1, j) arr
                . flip (M.insert (i,j)) sumA
                $ arr A.! (i,j)
                - sumA M.! (i-1, j-1)
                + sumA M.! (i-1, j)
                + sumA M.! (i, j-1)

        (_, (maxI, maxJ)) = A.bounds arr

sumValues :: Array (Int, Int) Int -> (Int, Int) -> (Int, Int) -> Int
sumValues sumA (sizeX, sizeY) (startX, startY) =
    sumA A.! (startX + sizeX - 1, startY + sizeY - 1)
    - sumA A.! (startX + sizeX - 1, startY - 1)
    - sumA A.! (startX - 1, startY + sizeY - 1)
    + sumA A.! (startX - 1, startY - 1)
