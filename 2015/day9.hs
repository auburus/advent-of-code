module Main where

import System.IO (readFile)
import Data.List
import Data.Array (Array, Ix)
import qualified Data.Array as A

data City = Tristram
          | AlphaCentauri
          | Snowdin
          | Tambi
          | Faerun
          | Norrath
          | Straylight
          | Arbre
          deriving (Eq, Ord, Ix, Show)

type CostMatrix = Array (City,City) Int

cities = [ Tristram, AlphaCentauri, Snowdin, Tambi
         , Faerun, Norrath, Straylight, Arbre ]

costMatrix :: [(City, City, Int)] -> CostMatrix
costMatrix = (A.//) baseMatrix .
             (++) sameCities .
             map (\(c1, c2, d) -> ((min c1 c2, max c1 c2), d)) 
    where
        sameCities = map (\c -> ((c,c), 0)) cities
        baseMatrix = A.listArray ((Tristram, Tristram), (Arbre,Arbre)) $ repeat (maxBound)

parseInput :: String -> (City, City, Int)
parseInput str = (city1, city2, dist)
    where
        splitted = words str
        city1 = parseCity (splitted !! 0)
        city2 = parseCity (splitted !! 2)
        dist = read (splitted !! 4)

parseCity city 
    | city == "Tristram" = Tristram
    | city == "AlphaCentauri" = AlphaCentauri
    | city == "Snowdin" = Snowdin
    | city == "Tambi" = Tambi
    | city == "Faerun" = Faerun
    | city == "Norrath" = Norrath
    | city == "Straylight" = Straylight
    | city == "Arbre" = Arbre

printList [] = return ()
printList (x:xs) = do
    print x
    printList xs

cost :: CostMatrix -> [City] -> Int
cost _ [] = 0
cost _ (_:[]) = 0
cost c (x:s@(y:_)) = c A.! (min x y, max x y) + cost c s

bruteforce :: CostMatrix -> [City] -> Int
bruteforce c = minimum .
               map (cost c) .
               permutations

part2 :: CostMatrix -> [City] -> Int
part2 c = maximum .
          map (cost c) .
          permutations

main = do
    contents <- readFile "input9.txt"
    let input = lines contents
        cMatrix = costMatrix . map parseInput $ input

    print . bruteforce cMatrix $ cities
    print . part2 cMatrix $ cities
