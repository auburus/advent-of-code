module Main where

import System.IO (readFile)
import Data.List
import KnotHash (knotHash)
import Numeric (showIntAtBase, readHex)
import Data.Char (intToDigit, digitToInt)
import Data.Array (Array)
import qualified Data.Array as A
import Data.Maybe (listToMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Queue (Queue)
import qualified Queue as Q
import Data.Foldable (toList)

main = do
    let input = "uugsqrei"

    print . doPart1 $ input
    print . doPart2 $ input

doPart1 :: String -> Int
doPart1 = sum
        . foldl (++) []
        . map (map digitToInt)
        . rows
        . map knotHash
        . inputList

inputList :: String -> [String]
inputList s = map (\x -> s ++ "-" ++ show x) [0,1..127]

hexToBin :: Char -> String
hexToBin c = showIntAtBase 2 intToDigit num ""
    where
        num = fst . head . readHex $ (c : []) :: Int

padZeros :: String -> String
padZeros s
    | length s < 4 = padZeros ('0' : s)
    | otherwise = s

rows :: [String] -> [String]
rows = map (foldl (++) [] . map (padZeros . hexToBin))


doPart2 :: String -> Int
doPart2 s = doCountGroups (buildGrid s) 0

buildGrid :: String -> Array (Int, Int) Int
buildGrid = A.array ((0,0), (127, 127))
        . foldl (++) []
        . map (\(i,xs) -> zipWith (\j x -> ((j,i), x)) [0,1..127] xs)
        . zip [0,1..127]
        . map (map digitToInt)
        . rows
        . map knotHash
        . inputList

doCountGroups :: Array (Int, Int) Int -> Int -> Int
doCountGroups grid i
    | S.null group = i
    | otherwise = doCountGroups grid' (i+1)
    where
        group = findGroup grid
        grid' = grid A.// [(x, 0) | x <- toList group]


firstOccupied :: Array (Int, Int) Int -> Maybe (Int, Int)
firstOccupied grid = 
    let maybeHead = listToMaybe . dropWhile (\(_, a) -> a == 0) . A.assocs
    in case maybeHead grid of
        Just h -> Just (fst h)
        Nothing -> Nothing

findGroup :: Array (Int, Int) Int -> Set (Int, Int)
findGroup grid =
    case firstOccupied grid of
        Just (x,y) -> doFindGroup grid (S.singleton (x,y)) (Q.singleton (x,y))
        Nothing -> S.empty

doFindGroup :: Array (Int, Int) Int -> Set (Int, Int) -> Queue (Int, Int) -> Set (Int, Int)
doFindGroup grid set queue
    | Q.null queue = set
    | otherwise = doFindGroup grid set' queue''
    
    where
        (front, queue') = Q.pop queue
        next = filter (\x -> grid A.! x == 1)
             . filter (\x -> S.notMember x set)
             $ nextItems front
        queue'' = Q.pushList queue' next
        set' = foldl (\s x -> S.insert x s) set next
                  


nextItems :: (Int, Int) -> [(Int, Int)]
nextItems (a,b) = filter (\(a,b) -> a >= 0 && a < 128 && b >= 0 && b < 128)
                $ [ (a+1, b)
                  , (a-1, b)
                  , (a, b+1)
                  , (a, b-1)
                  ]

