module Day01 where

import Data.Maybe (Maybe)
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import System.IO
import Data.List.Split (wordsBy)

type Rule = (Int, Int, Char)
type Password = String

main = do
  input <- fmap (map parseInput . lines) $ readFile "input02.txt"

  print $ problem1 input
  print $ problem2 input

problem1 = length . filter validPassword
problem2 = length . filter validPassword2

parseInput :: String -> (Rule, Password)
parseInput input = 
    let [min', max', letter, password] = wordsBy (flip elem "-: ") input
    in ((read min', read max', head letter), password)


validPassword :: (Rule, Password) -> Bool
validPassword ((min, max, letter), password)
  | quantity < min = False
  | quantity > max = False
  | otherwise = True
  where
    quantity = length . filter (==letter) $ password

validPassword2 :: (Rule, Password) -> Bool
validPassword2 ((pos1, pos2, letter), password) = 
  (==1) . length . filter (==letter) $ [letter1, letter2]
  where
    letter1 = password !! (pos1 - 1)
    letter2 = password !! (pos2 - 1)