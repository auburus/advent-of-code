module Main where

import System.IO (readFile)
import Data.List.Split (splitOn)
import Data.List
import Data.Function (on)
import Data.Array (Array)
import qualified Data.Array as A

data Pos = Pos Int Int deriving (Eq, Show)
data Node = Node Pos Int Int deriving (Show)
data Status = Empty
            | Normal
            | Full
            | Target
            deriving (Eq, Show)

type Grid = Array (Int, Int) Status

dataFit :: Node -> Node -> Bool
dataFit (Node _ _ avail) (Node _ size _)
    | size == 0 = False
    | otherwise = size <= avail

viableNodes :: [Node] -> [(Node, Node)]
viableNodes [] = []
viableNodes (x:xs) = zip (repeat x) (filter (dataFit x) xs) ++
                     zip (filter (\node -> dataFit node x) xs) (repeat x) ++
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

startNode :: [Node] -> Node
startNode xs = node
    where
        Just node = find (\(Node (Pos x y) _ _) -> x == 30 && y == 0) xs

-- Includes own node
adjacent :: [Node] -> Node -> [Node]
adjacent xs (Node (Pos x y) _ _) = filter isAdjacent xs
    where
        isAdjacent (Node (Pos a b) _ _) =
            a >= (x-1) && a<= (x+1) && b >= (y-1) && b <= (y+1)

bySize :: [Node] -> [Node]
bySize = sortBy (compare `on` (\(Node _ used avail) -> used))

printList :: Show a => [a] -> IO ()
printList [] = return ()
printList (x:xs) = do
    print x
    printList xs

printGrid :: Grid -> IO ()
printGrid = doPrint 0 . A.assocs
    where
        doPrint _ [] = return ()
        doPrint i xs = do
            print' current
            doPrint (i+1) other
            where
                (current, other) = partition (\((a, _), _) -> a == i) xs
                print' = print . map mapStatus
                    where
                        mapStatus (_, Empty) = '_'
                        mapStatus (_, Normal) = '.'
                        mapStatus (_, Full) = '#'
                        mapStatus (_, Target) = 'G'

mapToStatus :: Node -> ((Int, Int), Status)
mapToStatus (Node (Pos x y) used _) = ((x,y), status)
    where
        status
            | used == 0 = Empty
            | used > 450 = Full
            | x == 30 && y == 0 = Target
            | otherwise = Normal

findEmpty :: Grid -> ((Int, Int), Status)
findEmpty grid = result
    where
        Just result = find target . A.assocs $ grid
        target (_, Empty) = True
        target (_, _) = False

problem1 :: [String] -> Int
problem1 input = length . viableNodes . map parseInput $ input

-- Must move the blank from origin (13,27) to (4,15) to pass the #,
-- and then to (29,0) to start moving the target.
-- Once it is in (29, 0), there is 1 move to swap with target, and then
-- 5 * 29
problem2 = (13 - 4) + (27 - 15) + (15 - 0) + (29 - 4) +
           1 +
           (5 * 29)

main = do
    contents <- readFile "input22.txt"
    let input = tail . lines $ contents
        nodes = map parseInput input
        grid = A.array ((0,0),(30,30)) $ map mapToStatus nodes
 
    print . problem1 $ input
    printGrid grid -- I should transpose it, but whatever...
    print . findEmpty $ grid
    print problem2

