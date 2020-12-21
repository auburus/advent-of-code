module Day07 where

import Data.Array (Array)
import qualified Data.Array as Array
import Data.List
import Data.List.Split (splitOn, wordsBy)
import Data.Maybe (Maybe)
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import System.IO

type Bag = String

type Rule = (Bag, [(Int, Bag)])

main = do
  input <- fmap (map parseLine . map words . lines) $ readFile "input07.txt"

  print $ problem1 input
  print $ problem2 input

problem1 input = flip (-) 1 . length $ bagsThatContain input ["shinygold"]

problem2 input = numberOfBagsInBag input "shinygold"

parseLine :: [String] -> Rule
parseLine line =
  let bag = foldl (++) "" . take 2 $ line
      bags = parseBags $ drop 4 line
   in (bag, bags)

parseBags :: [String] -> [(Int, Bag)]
parseBags [] = []
parseBags (quantity : bag_1 : bag_2 : _ : xs) = (read quantity, bag_1 ++ bag_2) : parseBags xs
parseBags ("no" : "other" : "bags." : xs) = parseBags xs

bagsThatDirectlyContain :: [Rule] -> Bag -> [Bag]
bagsThatDirectlyContain rules bag = map fst . filter (\(_, xs) -> bag `elem` (map snd xs)) $ rules

bagsThatContain :: [Rule] -> [Bag] -> [Bag]
bagsThatContain rules bags = bagsThatContain' [] bags
  where
    bagsThatContain' bags_added [] = bags_added
    bagsThatContain' bags_added (bag : bags_to_add) =
      bagsThatContain' (bag : bags_added) (bags_to_add ++ (filter (not . flip elem (bags_added ++ bags_to_add)) $ bagsThatDirectlyContain rules bag))

numberOfBagsInBag :: [Rule] -> Bag -> Int
numberOfBagsInBag rules bag =
  sum . map (\(quantity, b) -> quantity + quantity * (numberOfBagsInBag rules b)) $ bags_contained
  where
    bags_contained = snd . head . filter ((== bag) . fst) $ rules