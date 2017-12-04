module Main where

import System.IO
import Data.List
import Data.Array(Array)
import Data.Array as A


type Dir = (Integer, Integer) -> (Integer, Integer)
type Grid = Array (Integer, Integer) Integer

up :: Dir
up = \(a,b) -> (a, b+1)

down :: Dir
down = \(a,b) -> (a, b-1)

left :: Dir
left = \(a,b) -> (a-1, b)

right :: Dir
right = \(a,b) -> (a+1, b)


main = do
    let input = 325489

    print . doPart1 $ input
    print . doPart2 $ input

doPart1 :: Integer -> Integer
doPart1 input = 
    let (x,y) = positions !! (fromIntegral input - 1 )
    in abs x + abs y

doPart2 :: Integer -> Integer
doPart2 input =
    let g = fullGrid grid 
        nums = map (\p -> g A.! p) positions
    in
        head . 
        dropWhile (\x -> x < input) $
        nums


move :: Dir -> (Integer, Integer) -> [(Integer, Integer)]
move dir (x, y) =
    let m = max (abs x) (abs y)
    in
        tail .
        takeWhile (\(a, b) -> max (abs a) (abs b) <= m) .
        iterate dir
        $ (x,y)

positions :: [(Integer, Integer)]
positions = (0,0) : positions' (0,0)

positions' :: (Integer, Integer) -> [(Integer, Integer)]
positions' (x, y) =
    let first = (x + 1,y)
        moveUp = move up first
        moveLeft = move left (last moveUp)
        moveDown = move down (last moveLeft)
        moveRight = move right (last moveDown)
        l = last moveRight
    in
        first : moveUp ++ moveLeft ++ moveDown ++ moveRight ++ positions' l


extend :: Grid -> Grid
extend grid =
    let (minBound, maxBound ) =
            (\((a,b), (a',b')) -> ((a-1, b-1), (a'+1, b'+1)))
            $ A.bounds grid
    in
        A.listArray (minBound, maxBound) [0,0..] A.// A.assocs grid

grid :: Grid
grid = (A.listArray ((-49,-49), (49,49)) [0,0..]) A.// [((0,0), 1)]
-- grid = (A.listArray ((-2, -2), (2, 2)) [0,0..]) A.// [((0,0), 1)]

fullGrid :: Grid -> Grid
fullGrid grid = 
    let pos = drop 1 . take (length $ A.indices grid) $ positions
    in
        foldl (\g p -> updateGrid g p) grid pos

updateGrid :: Grid -> (Integer, Integer) -> Grid
updateGrid g pos = 
    let neighbours = neighboursInGrid pos g
        values = map (\x -> g A.! x) neighbours
        sum = foldl (+) 0 values
    in
        g A.// [(pos, sum)]

neighboursInGrid :: (Integer, Integer) -> Grid -> [(Integer, Integer)] 
neighboursInGrid (x, y) g =
    let n = [ (x + 1, y)
            , (x + 1, y + 1)
            , (x + 1, y - 1)
            , (x, y + 1)
            , (x, y - 1)
            , (x - 1, y)
            , (x - 1, y + 1)
            , (x - 1, y - 1)
            ]
        ((minX, minY), (maxX, maxY)) = A.bounds g
    in
        filter (\(x,y) -> minX <= x && x <= maxX &&
                          minY <= y && y <= maxY ) n
