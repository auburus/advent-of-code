module Main where

import System.IO (readFile)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Array (Array)
import qualified Data.Array as A
import Data.Char (digitToInt)
import Data.List (permutations)

data Type = Free
          | Wall
          | Number Int
          deriving (Show, Eq, Ord)

type Pos = (Int, Int)
type Moves = Int
type HVAC = Array Pos Type
type Visited = Array Pos Bool

toVisited :: HVAC -> Visited
toVisited hvac = A.listArray (A.bounds hvac) $ repeat False

toHVAC :: String -> HVAC
toHVAC str = A.array ((0,0), (rows, cols)) $ mapHvac 0 l
    where
        l = lines str
        (cols, rows) = ((length . head) l -1, length l - 1)

        mapHvac row [] = []
        mapHvac row (x:xs) = (mapRow row 0 x) ++ mapHvac (row+1) xs

        mapRow row col [] = []
        mapRow row col (x:xs)
            | x == '#' = ((row, col), Wall) : remaining
            | x == '.' = ((row, col), Free) : remaining
            | otherwise = ((row, col), Number (digitToInt x)) : remaining
            where
                remaining = mapRow row (col+1) xs

nextPos :: Pos -> [Pos]
nextPos (x, y) = [ (x+1, y)
                 , (x-1, y)
                 , (x, y+1)
                 , (x, y-1)
                 ]

numbersPos :: HVAC -> [(Pos, Type)]
numbersPos = filter isNumber . A.assocs
    where
        isNumber (pos, Number i) = True
        isNumber (_, _) = False

-- Return them sorted by distance (nice effect of bfs)
distancesFromPos :: HVAC -> Pos -> [(Moves, Type)]
distancesFromPos hvac pos = bfs hvac (toVisited hvac A.// [(pos, True)]) [(0, pos)]

distances :: HVAC -> [(Int, Int, Moves)]
distances hvac = foldl (++) [] . map distances $ numbersPos hvac
    where
        distances (pos, Number i) =
            map (\(moves, Number j) -> (i, j, moves)) $ distancesFromPos hvac pos

bfs :: HVAC -> Visited -> [(Moves, Pos)] -> [(Moves, Type)]
bfs hvac visited [] = []
bfs hvac visited ((moves, pos):queue) =
    case (hvac A.! pos) of
        Wall -> bfs hvac visited queue
        Free -> bfs hvac visited' queue'
        Number i -> (moves, Number i) : bfs hvac visited' queue'
            
    where
        next = filter (\pos -> not (visited A.! pos)) $ nextPos pos
        nextVisited = map (\pos -> (pos, True)) next
        visited' = visited A.// nextVisited
        queue' = queue ++ map (\pos -> (moves +1, pos)) next

distMap :: HVAC -> Map (Int, Int) Moves
distMap =
    foldl (\map (i, j, moves) -> Map.insert (i,j) moves map) Map.empty . distances

routes :: Int -> [[Int]]
routes n = map (\x -> 0: x) $ permutations [1..n]

routeDist :: Map (Int, Int) Moves -> [Int] -> Moves
routeDist distMap (_:[]) = 0
routeDist distMap (x:s@(y:_)) =
    case Map.lookup (x,y) distMap of
        Nothing -> maxBound
        Just dist -> dist + routeDist distMap s

problem1 :: HVAC -> Int
problem1 hvac = minimum . map (routeDist (distMap hvac)) . routes $ 7

problem2 :: HVAC -> Int
problem2 hvac = minimum . map (routeDist (distMap hvac)) . routes' $ 7
    where
        routes' = map (\x -> x ++ [0]) . routes

main = do
    contents <- readFile "input24.txt"
    let contents' = "###########\n#0.1.....2#\n#.#######.#\n#4.......3#\n###########"
        hvac = toHVAC contents

    print . problem1 $ hvac
    print . problem2 $ hvac
