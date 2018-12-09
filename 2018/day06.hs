module Main where

import Data.Char
import System.IO (readFile)
import Data.Text (split, splitOn, pack, unpack)
import Data.List
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.Array (Array)
import qualified Data.Array as A
import Control.Applicative
import Data.Maybe (catMaybes)

main :: IO ()
main = do
    input <- map parseInput . lines <$> readFile "input06.txt"

    print $ problem1 input
    print $ problem2 input

problem1 input = 
    let (maxX, maxY) = (maximum $ map fst input, maximum $ map snd input)
        (minX, minY) = (minimum $ map fst input, minimum $ map snd input)
        infinite = nub . catMaybes . map (closest input)
            $ [(x,y) | x <- [minX, maxX], y <- [minY..maxY]] 
            ++ [(x,y) | x <- [minX..maxX], y <- [minY, maxY]]
    in
        M.fold (max) 0
        . M.filterWithKey (\x _ -> x `notElem` infinite)
        . foldl (\map a -> M.insertWith (+) a 1 map) M.empty
        . catMaybes
        . map (closest input)
        $ [(a,b) | a <- [minX..maxX], b <- [minY..maxY]]
        

closest :: [(Int, Int)] -> (Int, Int) -> Maybe (Int, Int)
closest list (a,b)
    | fst (close !! 0) == fst (close !! 1) = Nothing
    | otherwise = Just (list !! (snd . head $ close))

    where
        close = sort
              . (flip zip) [0,1..]
              . map (dist (a,b)) $ list

dist :: (Int, Int) -> (Int, Int) -> Int
dist (x,y) (a,b) = abs (x-a) + abs (y-b)

problem2 input =
    let startX = div (sum . map fst $ input) (length input)
        startY = div (sum . map snd $ input) (length input)
    in S.size . genArea input $ (startX, startY)
    

genArea :: [(Int, Int)] -> (Int, Int) -> Set (Int, Int)
genArea coords start = doGenArea [start] S.empty
    where
        doGenArea [] set = set
        doGenArea ((x,y):xs) set =
            let newCoords = filter (\(x,y) -> S.notMember (x,y) set
                    && (totalDist coords (x,y)) < 10000)
                    $ [(x+1,y), (x-1, y), (x, y+1), (x, y-1)]
            in
                doGenArea (xs ++ newCoords)
                . foldl (flip S.insert) set
                $ newCoords 


totalDist :: [(Int, Int)] -> (Int, Int) -> Int
totalDist coords x = sum . map (dist x) $ coords


parseInput :: String -> (Int, Int)
parseInput input = mymap . map unpack . split ((flip elem) ", ") . pack $ input
    where
        mymap [x, _, y] = (read x, read y)
