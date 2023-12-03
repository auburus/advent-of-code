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

-- data Value = Literal Char | Expr [Int] deriving (Eq, Show)
-- 
-- type Rule = (Int, Value)
-- 
-- -- type Rules = Map Int [Value]
-- 
-- type Message = String
-- 
-- 
-- 
-- 
-- parseRule :: String -> (Int, Rule)
-- parseRule str =
--   let (ix : str' : []) = splitOn ":" str
--       rule = parseRule' str'
--    in (read ix, rule)
-- 
-- parseRule' :: String -> Rule
-- parseRule' str
--   | '"' `elem` str = Literal . head . (!! 1) $ splitOn "\"" str
--   | otherwise = Expr . map (map read . words) $ splitOn "|" str
-- 
-- isMatch :: Rules -> Rule -> Message -> Bool
-- isMatch rules rule message = True
-- 
-- 
-- -- problem1 (rules, messages) = sort $ rulesUsedByRule rules 0
-- problem1 (rules, messages) = matchRule rules
-- 
-- -- rulesUsedByRule :: Rules -> Int -> [Int]
-- -- rulesUsedByRule rules n
-- --   | isExpr subrules = n : (nub . foldl (++) [] . map (rulesUsedByRule rules) . foldl (++) [] . (\(Expr a) -> a) $ subrules)
-- --   | otherwise = [n]
-- --   where
-- --     subrules = rules Map.! n
-- --     isExpr (Expr _) = True
-- --     isExpr _ = False
-- 
-- 
-- readInput :: String -> IO (Rules, [Message])
-- readInput f = do
--   (rawrules : rawmessages : []) <- fmap (splitOn "\n\n") $ readFile f
--   let rules = Map.fromList . map parseRule $ lines rawrules
--       messages = lines rawmessages

--   return (rules, messages)

-- readInput :: String -> IO (String)
readInput f = do
  (rawrules : rawmessages : []) <- fmap (splitOn "\n\n") $ readFile f

  return (rawrules, lines rawmessages)


main = do
  input <- readInput "input19.txt"

  print $ problem1 input

problem1 = id

data Value = Literal Char | Expr [Int] deriving (Eq, Show)

type Rules = [(Int, Value)]

type Message = String

getValuesForRule :: Rules -> Int -> [Value]
getValuesForRule rules r = map snd . filter ((==r) . fst) $ rules

matchRule :: Rules -> Value -> Message -> Maybe Message
matchRule _ (Literal c) (x : xs)
  | c == x = Just xs
  | otherwise = Nothing

matchRule _ (Expr []) xs = [xs]
matchRule rules (Expr (r:rs)) xs = 
  where
    values = getValuesForRule rules r
    matches = map (matchRule rules rs) values
