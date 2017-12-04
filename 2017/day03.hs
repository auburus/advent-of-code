module Main where

import System.IO
import Data.List

type Dir = (Integer, Integer) -> (Integer, Integer)

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

doPart1 :: Int -> Integer
doPart1 input = 
    let (x,y) = ((0,0) : positions (0,0)) !! (input - 1 )
    in abs x + abs y

move :: Dir -> (Integer, Integer) -> [(Integer, Integer)]
move dir (x, y) =
    let m = max (abs x) (abs y)
    in
        tail .
        takeWhile (\(a, b) -> max (abs a) (abs b) <= m) .
        iterate dir
        $ (x,y)

positions :: (Integer, Integer) -> [(Integer, Integer)]
positions (x, y) =
    let first = (x + 1,y)
        moveUp = move up first
        moveLeft = move left (last moveUp)
        moveDown = move down (last moveLeft)
        moveRight = move right (last moveDown)
        l = last moveRight
    in
        first : moveUp ++ moveLeft ++ moveDown ++ moveRight ++ positions l

