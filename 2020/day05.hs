module Day05 where

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
  input <- fmap lines $ readFile "input05.txt"

  print $ problem1 input

  print $ problem2 input

problem1 = maximum . map seatId

-- problem2 = seatId . snd . head . dropWhile (\(a, b) -> a == b) . zip allSeats . map rowCol
problem2 = seatId' . fst . head . dropWhile (\(a, b) -> a == b) . zip allSeats . sort . map rowCol

toBinary :: Char -> Int
toBinary 'F' = 0
toBinary 'B' = 1
toBinary 'L' = 0
toBinary 'R' = 1

binToDec :: [Int] -> Int
binToDec = snd . foldr (\x (idx, total) -> (idx + 1, x * round (2 ** idx) + total)) (0, 0)

seatId' :: (Int, Int) -> Int
seatId' (row, col) = row * 8 + col

seatId :: String -> Int
seatId seat =
  let (row, col) = rowCol seat
   in row * 8 + col

rowCol :: String -> (Int, Int)
rowCol seat =
  let row = binToDec . map toBinary $ take 7 seat
      col = binToDec . map toBinary $ drop 7 seat
   in (row, col)

allSeats :: [(Int, Int)]
allSeats = map (\x -> (x `div` 8, x `mod` 8)) [13, 14 ..]