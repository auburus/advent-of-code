module Main where

import Data.List
import System.IO (readFile)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Prop = Children
          | Cats
          | Samoyeds
          | Pomer
          | Akitas
          | Vizslas
          | Goldfish
          | Trees
          | Cars
          | Perfumes
          deriving (Eq, Ord, Show)

type Aunt = (Int, Map Prop Int)

parseInput :: String -> Aunt
parseInput str = (i, parseProps props)
    where
        splitted = words str
        i = read . init $ splitted !! 1
        props = drop 2 splitted
        parseProps :: [String] -> Map Prop Int
        parseProps [] = Map.empty
        parseProps (x:y:xs) = Map.insert key value (parseProps xs)
            where
                key = parseProp (init x)
                value = read . takeWhile (/=',') $ y
                              

parseProp :: String -> Prop
parseProp str
    | str == "children" = Children
    | str == "cats" = Cats
    | str == "samoyeds" = Samoyeds
    | str == "pomeranians" = Pomer
    | str == "akitas" = Akitas
    | str == "vizslas" = Vizslas
    | str == "goldfish" = Goldfish
    | str == "trees" = Trees
    | str == "cars" = Cars
    | str == "perfumes" = Perfumes

correctAunt :: [(Prop, Int)]
correctAunt = [ (Children, 3)
              , (Cats, 7)
              , (Samoyeds, 2)
              , (Pomer, 3)
              , (Akitas, 0)
              , (Vizslas, 0)
              , (Goldfish, 5)
              , (Trees, 3)
              , (Cars, 2)
              , (Perfumes, 1)
              ]

filters :: [(Prop, Int)] -> [(Aunt -> Bool)]
filters = map (\(prop, i) -> (\(_, aunt) -> case Map.lookup prop aunt of
                                                Just j -> i == j
                                                Nothing -> True
                                                ))

filters2 :: [(Prop, Int)] -> [(Aunt -> Bool)]
filters2 = map (\(prop, i) -> case prop of
                Cats ->     (\(_, aunt) -> maybe True (\j -> i < j) $ Map.lookup prop aunt)
                Trees ->    (\(_, aunt) -> maybe True (\j -> i < j) $ Map.lookup prop aunt)
                Pomer ->    (\(_, aunt) -> maybe True (\j -> i > j) $ Map.lookup prop aunt)
                Goldfish -> (\(_, aunt) -> maybe True (\j -> i > j) $ Map.lookup prop aunt)
                _ ->        (\(_, aunt) -> maybe True (\j -> i == j) $ Map.lookup prop aunt)
            )

validAunt :: [(Aunt -> Bool)] -> Aunt -> Bool
validAunt filt aunt = and $ map ($ aunt) filt

problem1 :: [Aunt] -> [Aunt]
problem1 = filter (validAunt $ filters correctAunt)

problem2 :: [Aunt] -> [Aunt]
problem2 = filter (validAunt $ filters2 correctAunt)

main = do
    contents <- readFile "input16.txt"
    let input = lines contents

    print . problem1 . map parseInput $ input
    print . problem2 . map parseInput $ input
