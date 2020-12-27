module Main where

import Data.Array (Array)
import qualified Data.Array as Array
import Data.List
import Data.List.Split (splitOn, wordsBy)
import Data.Maybe (Maybe)
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import System.IO

data Seat = Floor | Empty | Occupied deriving (Eq)

type Area = Array (Int, Int) Seat

instance Show Seat where
  show Floor = "."
  show Empty = "L"
  show Occupied = "#"

parseSeat :: Char -> Seat
parseSeat '.' = Floor
parseSeat 'L' = Empty

parseArea :: [[Seat]] -> Area
parseArea xs = Array.listArray ((0, 0), (length xs -1, length (head xs) -1)) $ foldl (++) [] xs

readInput :: String -> IO Area
readInput = fmap (parseArea . map (map parseSeat) . lines) . readFile

main = do
  input <- readInput "input11.txt"

  print $ problem1 input
  print $ problem2 input

problem1 = length . filter (== Occupied) . Array.elems . (findStable tic)

problem2 = length . filter (== Occupied) . Array.elems . (findStable tic')

findStable :: (Area -> Area) -> Area -> Area
findStable tic area = snd . head . dropWhile ((== True) . fst) $ zip hasChanged area'
  where
    hasChanged = zipWith (/=) area' $ drop 1 area'
    area' = iterate tic area

-- findStationary :: Area -> Area
-- -- findStationary area = fst . head . dropWhile (\(a, b) -> a /= b) . zip areas $ tail areas
-- findStationary area = fst . head . drop 100 . zip areas $ tail areas
--   where
--     areas = iterate tic area

-- changes :: [Area] -> [Int]
-- changes (x : y : xs) = map length . filter (\(a, b) -> (a /= b)) $ zip (Array.elems x) (Array.elems y)
changes (x : y : xs) = (length . filter (\(a, b) -> a /= b) $ zip (Array.elems x) (Array.elems y)) : changes (y : xs)

surrounding :: Area -> (Int, Int) -> [Seat]
surrounding area (x, y) =
  map ((Array.!) area)
    . filter (Array.inRange (Array.bounds area))
    $ [(i, j) | i <- [(x -1) .. (x + 1)], j <- [(y -1) .. (y + 1)], (i, j) /= (x, y)]

tic :: Area -> Area
tic area = area Array.// [(seat, seatTic area seat) | seat <- Array.range (Array.bounds area)]

seatTic :: Area -> (Int, Int) -> Seat
seatTic area (x, y)
  | seat == Empty && occ == 0 = Occupied
  | seat == Occupied && occ >= 4 = Empty
  | otherwise = seat
  where
    seat = area Array.! (x, y)
    surroundings = surrounding area (x, y)
    occ = length . filter (== Occupied) $ surroundings

tic' :: Area -> Area
tic' area = area Array.// [(seat, seatTic' area seat) | seat <- Array.range (Array.bounds area)]

seatTic' :: Area -> (Int, Int) -> Seat
seatTic' area (x, y)
  | seat == Empty && occ == 0 = Occupied
  | seat == Occupied && occ >= 5 = Empty
  | otherwise = seat
  where
    seat = area Array.! (x, y)
    surroundings = visibleSurrounding area (x, y)
    occ = length . filter (== Occupied) $ surroundings

visibleSurrounding :: Area -> (Int, Int) -> [Seat]
visibleSurrounding area (x, y) =
  Maybe.catMaybes
    . map (firstVisible area (x, y))
    $ [(i, j) | i <- [-1 .. 1], j <- [-1 .. 1], (i, j) /= (0, 0)]

firstVisible :: Area -> (Int, Int) -> (Int, Int) -> Maybe Seat
firstVisible area (x, y) dir =
  Maybe.listToMaybe
    . dropWhile (== Floor)
    . map ((Array.!) area)
    . takeWhile (Array.inRange (Array.bounds area))
    . drop 1
    $ iterate (addVector dir) (x, y)

addVector :: (Int, Int) -> (Int, Int) -> (Int, Int)
addVector (x, y) (a, b) = (x + a, y + b)

printArea :: Area -> String
printArea = unlines . map (foldl (++) [] . map snd) . groupBy (\x y -> (fst $ fst x) == (fst $ fst y)) . map (\(p, x) -> (p, show x)) . Array.assocs
