module Main where

import Data.Array (Array, Ix)
import qualified Data.Array as A
import Data.List

data Person = Alice
            | Bob
            | Carol
            | David
            | Eric
            | Frank
            | George
            | Mallory
            | Auburus
            deriving (Eq, Ord, Ix, Show)

type CostMatrix = Array (Person, Person) Int

people = [ Alice, Bob, Carol, David, Eric, Frank
         , George, Mallory]

createCost :: [((Person, Person), Int)] -> Array (Person, Person) Int
createCost = (A.//) baseCost
    where
        baseCost = A.listArray ((Alice, Alice), (Auburus, Auburus)) $ repeat 0

parseInput :: String -> ((Person, Person), Int)
parseInput str = ((p1, p2), i * val)
    where
        splitted = words str
        p1 = parseName (splitted !! 0)
        p2 = parseName . init . last $ splitted
        i = if (splitted !! 2) == "gain" then 1 else -1
        val = read (splitted !! 3)

parseName :: String -> Person
parseName str
    | str == "Alice" = Alice
    | str == "Bob" = Bob
    | str == "Carol" = Carol
    | str == "David" = David
    | str == "Eric" = Eric
    | str == "Frank" = Frank
    | str == "George" = George
    | str == "Mallory" = Mallory

happiness :: CostMatrix -> [Person] -> [Int]
happiness c persons = doHappiness c (persons ++ [(head persons)])
    where
        doHappiness _ [] = []
        doHappiness _ (_:[]) = []
        doHappiness c (x:s@(y:_)) = (c A.! (x,y)) :
                                    (c A.! (y,x)) :
                                    doHappiness c s

problem :: CostMatrix -> [Person] -> Int
problem c = maximum .
             map (sum . happiness c) .
             permutations

main = do
    contents <- readFile "input13.txt"
    let input = map parseInput . lines $ contents
        cMatrix = createCost input

    print . problem cMatrix $ people

    print . problem cMatrix $ (Auburus : people)
