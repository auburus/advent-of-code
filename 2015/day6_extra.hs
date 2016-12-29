module Main where

import Data.Array (Array)
import qualified Data.Array as A
import Data.List
import Data.List.Split (splitOn)

type Grid = Array Idx Int
type Idx = (Int, Int)

toggle :: Grid -> [Idx] -> Grid
toggle grid idx = grid A.// (map (\i -> (i, (grid A.! i) + 2)) idx)

turnOn :: Grid -> [Idx] -> Grid
turnOn grid idx = grid A.// (map (\i -> (i, (grid A.! i) + 1)) idx)

turnOff :: Grid -> [Idx] -> Grid
turnOff grid idx = grid A.// (map (\i -> (i, max ((grid A.! i) - 1) 0)) idx)

grid :: Grid
grid = A.listArray ((0,0), (999,999)) $ repeat 0

execute :: [String] -> Grid -> Grid
execute [] grid = grid
execute (ins:xs) grid
    | order == "turn" && second == "on" =
        execute xs . turnOn grid $ rectangle (drop 2 splitted)
    | order == "turn" && second == "off" =
        execute xs . turnOff grid $ rectangle (drop 2 splitted)
    | order == "toggle" =
        execute xs . toggle grid $ rectangle (drop 1 splitted)
    where
        splitted = words ins
        order = head splitted
        second = splitted !! 1

rectangle :: [String] -> [Idx]
rectangle splitted = rectangle' (parse (splitted !! 0)) (parse (splitted !! 2))
    where
        parse = tuplify . map read . splitOn ","
        tuplify (x:y:[]) = (x,y)
        rectangle' :: Idx -> Idx -> [Idx]
        rectangle' (x1, y1) (x2, y2) = [(x,y) | x <- [x1..x2], y <- [y1..y2]]

printList :: Show a => [a] -> IO ()
printList [] = return ()
printList (x:xs) = do
    print x
    printList xs

problem :: [String] -> Int
problem ins = sum . A.elems . execute ins $ grid

main = do
    contents <- readFile "input6.txt"
    let contents' = "turn on 0,0 through 999,999\ntoggle 0,0 through 999,0\nturn off 499,499 through 500,500\ntoggle 498,498 through 499,499"

    print . problem . lines $ contents
