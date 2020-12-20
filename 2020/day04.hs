module Day04 where

import Data.Array (Array)
import qualified Data.Array as Array
import Data.List
import Data.List.Split (splitOn, wordsBy)
import Data.Maybe (Maybe)
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import System.IO

main = do
  input <- fmap (map parseBlock . splitOn "\n\n") $ readFile "input04.txt"

  print $ problem1 input
  print $ problem2 input

problem1 = length . filter isValid

problem2 = length . filter (foldl (&&) True . map isValidField) . filter (isValid)

tuplify [x, y] = (x, y)

parseBlock :: String -> [(String, String)]
parseBlock = map (tuplify . splitOn ":") . wordsBy (flip elem "\n ")

isValid :: [(String, String)] -> Bool
isValid x
  | length x == 8 = (sort . map fst $ x) == ["byr", "cid", "ecl", "eyr", "hcl", "hgt", "iyr", "pid"]
  | length x == 7 = (sort . map fst $ x) == ["byr", "ecl", "eyr", "hcl", "hgt", "iyr", "pid"]
  | otherwise = False

isValidField :: (String, String) -> Bool
isValidField (key, value)
  | key == "byr" && read value >= 1920 && read value <= 2002 = True
  | key == "iyr" && read value >= 2010 && read value <= 2020 = True
  | key == "eyr" && read value >= 2020 && read value <= 2030 = True
  | key == "hgt" && validHeight value = True
  | key == "hcl" && validHairColor value = True
  | key == "ecl" && value `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"] = True
  | key == "pid" && (length value == 9) && all (flip elem "0123456789") value = True
  | key == "cid" = True
  | otherwise = False

validHeight h
  | unit == "cm" && read value >= 150 && read value <= 193 = True
  | unit == "in" && read value >= 59 && read value <= 76 = True
  | otherwise = False
  where
    (value, unit) = span (flip elem "0123456789") h

validHairColor value = (length value == 7) && head value == '#' && all (flip elem "#0123456789abcdef") value
