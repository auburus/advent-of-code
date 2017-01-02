module Main where

import Data.Array (Array)
import qualified Data.Array as A

data Light = Off
           | On
           deriving (Show, Eq)

type Grid = Array (Int, Int) Light


neighbours :: (Int, Int) -> [(Int, Int)]
neighbours (x,y) = [(x',y') | x' <- [x-1,x,x+1], y' <- [y-1,y,y+1],
                    x' >= 0, x' <= 99, y' >= 0, y' <= 99, not (x == x' && y == y')]

update :: Grid -> ((Int, Int), Light) -> ((Int, Int), Light)
update grid (pos, l) =
    case l of
        On
            | onNeighbours == 2 || onNeighbours == 3 -> (pos, On)
            | otherwise -> (pos, Off)
        Off
            | onNeighbours == 3 -> (pos, On)
            | otherwise -> (pos, Off)
    where
        onNeighbours = length . filter (\p -> (grid A.! p) == On) $ neighbours pos

baseGrid :: Int -> Int -> Grid
baseGrid i j = A.listArray ((0,0), (i,j)) $ repeat Off

step :: Grid -> Grid
step g = (A.//) g . map (update g) . A.assocs $ g

updateCorners :: Grid -> Grid
updateCorners g = g A.// [ ((0,0), On)
                         , ((0, 99), On)
                         , ((99, 0), On)
                         , ((99,99), On)]

parseInput :: String -> [Light]
parseInput [] = []
parseInput (x:xs)
    | x == '#' = On : parseInput xs
    | otherwise = Off : parseInput xs

problem1 :: Grid -> Int
problem1 = length .
           filter (==On) .
           A.elems .
           last .
           take 101 .
           iterate step

problem2 :: Grid -> Int
problem2 = length .
           filter (==On) .
           A.elems .
           last .
           take 101 .
           iterate (updateCorners . step)

main = do
    contents <- readFile "input18.txt"   
    let input = parseInput . foldl (++) [] . lines $ contents
        grid = A.listArray ((0,0),(99,99)) input

    print . problem1 $ grid
    print . problem2 . updateCorners $ grid
