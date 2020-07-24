module Day03 where

import System.IO
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List
import Data.Maybe (fromJust)
import Data.List.Split

-- input = "R8,U5,L5,D3\nU7,R6,D4,L4"

type Dir = Char
type Pos = (Integer, Integer)

-- input = "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83"
main = do
    input <- readFile "input03.txt"
    let input'= map parseLine . lines $ input
    print $ problem1 input'
    print $ problem2 input'


problem1 xs = minimum
            . map (\(a,b) -> abs a + abs b)
            $ intersections (path . head $ xs) (path . last $ xs)

problem2 xs = minimum $ zipWith (+) steps1 steps2
    where
        path1 = path $ head xs
        path2 = path $ last xs
        intersec = intersections path1 path2
        steps1 = map (steps path1) $ intersec
        steps2 = map (steps path2) $ intersec


parseLine = map parseInput . splitOn ","


parseInput :: String -> (Dir, Integer)
parseInput str = (a,b)
    where
    (a, tail) = fromJust . uncons $ str
    b = read tail


subpath :: Pos -> (Dir, Integer) -> [Pos]
subpath (x,y) ('U', distance) = [(x, y + i) | i <- [1..distance]]
subpath (x,y) ('D', distance) = [(x, y - i) | i <- [1..distance]]
subpath (x,y) ('L', distance) = [(x - i, y) | i <- [1..distance]]
subpath (x,y) ('R', distance) = [(x + i, y) | i <- [1..distance]]

visitedOneDir :: Pos -> (Dir, Integer) -> Set Pos
visitedOneDir pos (dir, dist) = Set.fromList $ subpath pos (dir, dist)

path :: [(Dir, Integer)] -> [Pos]
path = path' [(0, 0)]
    where
        path' ys [] = ys
        path' ys (x:xs) = path' (ys ++ subpath') xs
            where
                subpath' = subpath (last ys) x

intersections :: [Pos] -> [Pos] -> [Pos]
intersections path1 path2 =
    filter (/=(0, 0)) .
    Set.toList $ (Set.fromList path1) `Set.intersection` (Set.fromList path2)

steps :: [Pos] -> Pos -> Int
steps xs x = length $ takeWhile (/=x) xs

