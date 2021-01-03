module Main where

import Data.Array (Array)
import qualified Data.Array as Array
import Data.List
import Data.List.Split (splitOn, wordsBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (Maybe)
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import System.IO

data Token = Value Int | Open | Close | Plus | Times deriving (Eq)

instance Show Token where
  show (Value x) = show x
  show Open = "("
  show Close = ")"
  show Plus = "+"
  show Times = "*"

type Expression = [Token]

readInput :: String -> IO ([String])
readInput f = fmap (map (filter (/= ' ')) . lines) $ readFile f

main = do
  input <- readInput "input18.txt"

  print $ problem1 input
  print $ problem2 input

problem1 = sum . map (reduce . tokenize)

problem2 = sum . map (reduce' . tokenize)

step :: Expression -> Expression
step (Value x : []) = [Value x]
step (Value x : Plus : Value y : xs) = Value (x + y) : xs
step (Value x : Times : Value y : xs) = Value (x * y) : xs
step (Open : Value x : Close : xs) = Value x : xs
step (Value x : op : Open : xs) = Value x : op : (step $ Open : xs)
step (Open : xs) = Open : (step subexpr) ++ Close : ys
  where
    (subexpr, ys) = subexprInParens ([Open], xs)

subexprInParens :: (Expression, Expression) -> (Expression, Expression)
subexprInParens (inside, outside)
  | count Open inside == count Close inside = (init . tail $ inside, outside)
  | otherwise = subexprInParens (inside ++ (take 1 outside), tail outside)

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

reduce :: Expression -> Int
reduce = (\(Value a) -> a) . head . head . dropWhile ((/= 1) . length) . iterate step

tokenize :: String -> Expression
tokenize [] = []
tokenize ('(' : xs) = Open : tokenize xs
tokenize (')' : xs) = Close : tokenize xs
tokenize ('+' : xs) = Plus : tokenize xs
tokenize ('*' : xs) = Times : tokenize xs
tokenize xs = (\(a, b) -> Value (read a) : tokenize b) . span (flip elem "0123456789") $ xs

reduce' :: Expression -> Int
reduce' = (\(Value a) -> a) . head . head . dropWhile ((/= 1) . length) . iterate step'

step' :: Expression -> Expression
step' (Value x : []) = [Value x]
step' (Value x : Plus : Value y : xs) = Value (x + y) : xs
step' (Value x : Times : Value y : xs)
  | count Plus xs > 0 = Value x : Times : step' (Value y : xs)
  | otherwise = Value (x * y) : xs
step' (Open : Value x : Close : xs) = Value x : xs
step' (Value x : op : Open : xs) = Value x : op : (step' $ Open : xs)
step' (Open : xs) = Open : (step' subexpr) ++ Close : ys
  where
    (subexpr, ys) = subexprInParens ([Open], xs)