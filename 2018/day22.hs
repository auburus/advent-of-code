module Main where

import System.IO (readFile)
import Control.Applicative
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S


type Time = Int
type Pos = (Int, Int)
type Pos' = (Time, Pos, Tool)

data Tool = Torch | Climbing | Neither deriving (Show, Eq, Ord)

depth = 6084 :: Int
target = (14, 709) :: Pos

main = do
    let erosionMap = eroMap target
        erosionMap' = eroMap (150,750)

    print $ problem1 erosionMap'

problem1 :: Map Pos Int -> Int
problem1 = sum . M.elems

eroMap :: Pos -> Map Pos Int
eroMap pos = M.map (flip mod 3) 
    $ foldl erosion M.empty [(x,y) | x <- [0..(fst pos)], y <- [0.. (snd pos)] ]

erosion :: Map Pos Int -> Pos -> Map Pos Int
erosion map pos =
        M.insert pos (((geoIndex pos) + depth) `mod` 20183) map

    where
        geoIndex :: Pos -> Int
        geoIndex (0, 0) = 0
        geoIndex (14,709) = 0
        geoIndex (x, 0) = x * 16807
        geoIndex (0, y) = y * 48271
        geoIndex (x, y) = map M.! (x-1, y) * map M.! (x, y-1)


dijsktra :: Map Pos Int -> Set Pos -> Map Pos (Time, Tool) -> Pos' -> Pos'
dijsktra _ _ _ (t, (14,709), tool) =
    case tool of 
        Torch -> d
        _ -> 7 + t

dijsktra grid visited unvisited (time, pos, tool) = 
    dijsktra grid (S.insert pos visited) unvisited'
    where
        unvisited' = M.unionWith combine unvisited
                   . M.fromList
                   $ map 

-- This function may malfunction in case of a tie with different tools...
combine :: (Time, Tool) -> (Time, Tool) -> (Time, Tool)
combine (t1, tool1) (t2, tool2)
    | t1 < t2 = (t1, tool1)
    | t1 == t2 && (tool1 /= tool2) = error "Using different tools"
    | otherwise = (t2, tool2)



neighbours :: Map Pos Int -> (Time, Pos, Tool) -> [(Pos, (Time, Tool))]
neighbours grid (t, pos, tool) =
    where
        nPos (x,y) = filter (\(a,b) -> a >= 0 && b >= 0) 
                   $ [(x, y-1), (x-1, y), (x+1, y), (x, y+1)]


time :: Map Pos Int -> (Pos, Tool) -> Pos -> (Time, Tool)
time grid (pos1, tool1) (pos2, tool2)
