module Main where

import System.IO (readFile)
import Data.Array (Array)
import qualified Data.Array as A

type Coord = (Int, Int)
type Diagram = Array Coord Char
data Dir = N | S | E | W deriving (Eq, Show)

main = do
    contents <- readFile "input19.txt"

    let input = lines contents

    print . doPart1 $ input
    print . doPart2 $ input


doPart1 :: [String] -> [Char]
doPart1 input =
    let diagram = inputToDiagram input
        path = follow diagram $ findStartPos diagram
    in
        init
        . filter (\a -> a `notElem` ['|', '-', '+'] )
        . map ((A.!) diagram)
        $ path

doPart2 :: [String] -> Int
doPart2 input =
    let diagram = inputToDiagram input
        path = follow diagram $ findStartPos diagram
    in
        length path - 1

inputToDiagram :: [String] -> Diagram
inputToDiagram input =
    let width = length . head $ input
        height = length $ input

    in
        A.array ((0, 0), (height - 1, width - 1))
        $ [((i, j), (input !! i) !! j) | i <- [0..(height - 1)]
                                       , j <- [0..(width - 1)]]

findStartPos :: Diagram -> (Coord, Dir)
findStartPos diagram = (fst . head . filter (\((i,_),a) -> i == 1 && a /= ' ') . A.assocs $ diagram, S)

findNextPos :: Diagram -> (Coord, Dir) -> Maybe (Coord, Dir)
findNextPos diagram (p@(i,j), dir)
    | diagram A.! p == '|' || diagram A.! p == '-' =
        case dir of
            N -> Just ((i-1, j), N)
            S -> Just ((i+1, j), S)
            E -> Just ((i, j+1), E)
            W -> Just ((i, j-1), W)
    | diagram A.! p == '+' =
        case dir of
            N -> if (diagram A.! (i, j+1)) /= ' '
                 then Just ((i, j+1), E)
                 else Just ((i, j-1), W)
            S -> if (diagram A.! (i, j+1)) /= ' '
                 then Just ((i, j+1), E)
                 else Just ((i, j-1), W)
            E -> if (diagram A.! (i-1, j)) /= ' '
                 then Just ((i-1, j), N)
                 else Just ((i+1, j), S)
            W -> if (diagram A.! (i-1, j)) /= ' '
                 then Just ((i-1, j), N)
                 else Just ((i+1, j), S)
    | diagram A.! p /= ' ' =
        case dir of
            N -> Just ((i-1, j), N)
            S -> Just ((i+1, j), S)
            E -> Just ((i, j+1), E)
            W -> Just ((i, j-1), W)

    | diagram A.! p == ' ' = Nothing

follow :: Diagram -> (Coord, Dir) -> [Coord]
follow diagram (p, dir) =
    case findNextPos diagram (p, dir) of
        Nothing -> [p]
        Just (p', dir') -> p : (follow diagram (p', dir'))
