module Main where

import System.IO (readFile)
import Data.List
import Control.Applicative
import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as S


type Coords = [Int]
type Constellation = [Coords]

main = do
    input <- map (map read) . map (splitOn ",") . lines <$> readFile "input25.txt" :: IO [[Int]]
    print $ problem1 input


problem1 = length . constellations

dist [] [] = 0
dist (x:xs) (y:ys) = abs (x - y) + dist xs ys

constellations :: [Coords] -> [Constellation]
constellations xs = run xs []
    where
        run :: [Coords] -> [Constellation] -> [Constellation]
        run [] cs = cs
        run (x:xs) ys = (uncurry run)
                      . (\(a,b) -> (a, (b:ys)))
                      $ findConst [x] xs []


findConst :: [Coords] -> [Coords] -> Constellation -> ([Coords], Constellation)
findConst [] others cs = (others, cs)
findConst (x:xs) ys cs = findConst (xs++new) others (x:cs)
    where
        (new, others) = partition ((<=3) . dist x) ys
