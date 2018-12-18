module Main where

import System.IO (readFile)
import Data.List
import Control.Applicative
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as S

main = do
    input <- parseInput <$> readFile "input18.txt"

    print $ problem1 10 input
    print $ problem2 1000000000 input



problem1 :: Int -> Map (Int, Int) Char -> Int
problem1 0 area = countWooded area * countLumber area
problem1 n area = problem1 (n-1) $ M.mapWithKey (change area) area

problem2 n area = 
    let seed = iter 600 area
        patt = findPattern (seed) S.empty
    in 
        problem1 0 (iter ((n-600) `mod` patt) seed)

iter 0 area = area
iter n area = iter (n-1) $ M.mapWithKey (change area) area

countWooded = M.size . M.filter (=='|')
countLumber = M.size . M.filter (=='#')


change :: Map (Int, Int) Char -> (Int, Int) -> Char -> Char
change area pos c =
    let adj = adjacent area pos
    in 
    case c of
        '.' | (>=3) . length . filter (=='|') $ adj -> '|'
            | otherwise -> '.'

        '|' | (>=3) . length . filter (=='#') $ adj -> '#'
            | otherwise -> '|'

        '#' | '#' `elem` adj && '|' `elem` adj -> '#'
            | otherwise -> '.'
 
findPattern :: Map (Int, Int) Char -> Set String -> Int
findPattern area seen
    | (toString area) `S.member` seen = S.size seen
    | otherwise = findPattern (M.mapWithKey (change area) area)
                $ S.insert (toString area) seen

adjacent :: Map (Int, Int) Char -> (Int, Int) -> [Char]
adjacent area (x,y) =
    catMaybes . map (flip M.lookup area)
    $ [(x+a, y+b) | a <- [-1, 0, 1], b <- [-1, 0, 1], (a,b) /= (0,0)]
    -- $ [(x-1,y), (x+1, y), (x, y-1), (x, y+1)

parseInput :: String -> Map (Int, Int) Char
parseInput str = M.fromList 
               . zip [(j, i) | i <- [0..(length . lines $ str)-1]
                            , j <- [0..(length . head . lines $ str)-1]]
               . foldl1 (++)
               $ lines str

toString :: Map (Int, Int) Char -> String
toString = foldl (flip (:)) [] . map snd . sort . M.assocs

printArea :: Map (Int, Int) Char -> IO ()
printArea area =
    let idx = map (\i -> [(j, i) | j <- [0..49]]) $ [0..49] :: [[(Int, Int)]]
    in do
        mapM_ putStrLn . map (map ((M.!) area)) $ idx
     
