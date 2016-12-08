module Main where

import System.IO
import Data.Array
import Data.Char
import Data.List.Split

type Screen = Array (Int, Int) Char

type Part = [((Int, Int), Char)]

getRow :: Int -> Screen -> Part
getRow row = filter (\((i,_), _) -> i == row) . assocs

getRow' :: Screen -> Int -> Part
getRow' screen row = getRow row screen

getCol :: Int -> Screen -> Part
getCol col = filter (\((_,j), _) -> j == col) . assocs 

getRows :: Screen -> [Part]
getRows screen =
    let (_, (maxRow, _)) = bounds screen
    in
        map (getRow' screen) [0..maxRow]

rect :: Int -> Int -> Part
rect x y =
    let vectorial :: [Int] -> [Int] -> [(Int, Int)]
        vectorial [] _ = []
        vectorial _ [] = []
        vectorial (x:xs) ys = (map (\i -> (x, i)) ys) ++ vectorial xs ys
    in
        zip (vectorial [0..y] [0..x]) ['#','#'..]

rotate :: Part -> Int -> Part
rotate xs n = zip pos (rotate' val n)
    where
        (pos, val) = unzip xs
        rotate' :: [Char] -> Int -> [Char]
        rotate' xs 0 = xs
        rotate' xs n = rotate' (last xs : init xs) (n-1)
        

execRect :: Screen -> [String] -> Screen
execRect screen instructions = 
    let dimensions = last instructions
        (x,y) = ((\[a,b] -> (a-1, b-1)) . map read . splitOn "x") dimensions
    in
        screen // (rect x y)

execRotate :: Screen -> [String] -> Screen
execRotate screen instructions
    | part == "row" = screen // (rotate ( getRow idx screen) value)
    | part == "column" = screen // (rotate ( getCol idx screen) value)
    | otherwise = error "Invalid rotation"
        where
            part = instructions !! 1
            value = read (last instructions)
            idx = ((\[_,a] -> read a) . splitOn "=")  (instructions !! 2)

execInstruction :: Screen -> String -> Screen
execInstruction screen instruction
    | firstWord == "rect" = execRect screen instructions
    | firstWord == "rotate" = execRotate screen instructions
    | otherwise = error "Invalid instruction"
        where
            instructions = words instruction
            firstWord = head instructions

pixelsLit :: Screen -> Int
pixelsLit screen = foldl (+) 0 numeric
    where
        numeric = map (\x -> if x == '#' then 1 else 0) (elems screen)

printScreen :: Screen -> IO ()
printScreen screen =
    let
        rows = getRows screen
        printRows :: [Part] -> IO ()
        printRows [] = return ()
        printRows (x:xs) =
            do
                putStrLn $ values x
                printRows xs
            where values row = map snd row
    in
        printRows rows

main = do
    contents <- readFile "input8.txt"
    let contents' = "rect 3x2\nrotate column x=1 by 1\nrotate row y=0 by 4\nrotate column x=1 by 1"
        inputs = lines contents
        screen :: Screen
        -- screen = listArray ((0,0), (2,6)) ['.','.'..]
        screen = listArray ((0,0), (5, 49)) ['.','.'..]
        screens = screen : zipWith execInstruction screens inputs

    printScreen $ last screens
    print $ pixelsLit (last screens)

