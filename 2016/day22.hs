module Main where

import System.IO (readFile)
import Data.List.Split (splitOn)

data Pos = Pos Int Int deriving (Eq, Show)
data Node = Node Pos Int Int deriving (Show)

dataFit :: Node -> Node -> Bool
dataFit (Node _ _ avail) (Node _ size _)
    | size == 0 = False
    | otherwise = size <= avail

viableNodes :: [Node] -> Int
viableNodes [] = 0
viableNodes (x:xs) = (length . filter (dataFit x) $ xs) +
                     (length . filter (\node -> dataFit node x) $ xs) +
                     viableNodes xs

parseInput :: String -> Node
parseInput str =
    let splitted = words str
        [x,y] = map (read . tail) . splitOn "-" . head $ splitted
        used = parseSize (splitted !! 2)
        avail = parseSize (splitted !! 3)
        parseSize :: String -> Int
        parseSize = read . init
    in
        Node (Pos x y) used avail

problem1 :: [String] -> Int
problem1 input = viableNodes . map parseInput $ input

main = do
    contents <- readFile "input22.txt"
    let input = tail . lines $ contents

    print . problem1 $ input
