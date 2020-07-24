module Day04 where

import System.IO
import Data.List
import qualified Data.Set as Set

main = do
    let input = (128392, 643281) :: (Int, Int)
    print $ problem1 input
    print $ problem2 input



problem1 = length . subset1
problem2 = length . filter digitAppearsExactly2Times . subset1

subset1 (min, max) = 
    filter (\x-> x <= digits max)
    . filter (\x-> x >= digits min)
    . filter doNotDecrease
    $ haveDuplicated

digits :: Integral x => x -> [x]
digits 0 = []
digits x = digits (x `div` 10) ++ [x `mod` 10]

doNotDecrease :: [Int] -> Bool
doNotDecrease (x:[]) = True
doNotDecrease (x:y:xs)
    | x > y = False
    | otherwise = doNotDecrease (y:xs)


free = [digits x | x <- [1000..9999]]

pairs :: [[Int]]
pairs = [[0, 0]] ++ [digits (11*x) | x <- [1..9]]

haveDuplicated =
    Set.toList . Set.fromList
    $ [ (take i xs) ++ ys ++ (drop i xs) | i <- [0..4], xs <- free, ys <- pairs]

digitAppearsExactly2Times :: [Int] -> Bool
digitAppearsExactly2Times [] = False
digitAppearsExactly2Times (x:xs)
    | length (takeWhile (==x) (x:xs)) == 2 = True
    | otherwise = digitAppearsExactly2Times $ dropWhile (==x) xs

