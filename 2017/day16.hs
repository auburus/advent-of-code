module Main where

import System.IO (readFile)
import Data.List
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.List.Split (splitOn)
import Data.Foldable (toList)
import Data.Char (digitToInt)
import Data.Array (Array)
import qualified Data.Array as A

main = do
    contents <- readFile "input16.txt"
    let input = splitOn "," . head . lines $ contents

    print . doPart1 $ input
    print . doPart2 $ input

doPart1 :: [String] -> Seq Char
doPart1 = fullDance (Seq.fromList "abcdefghijklmnop")

doPart2 :: [String] -> Seq Char
doPart2 moves =
    let loop = findLoop moves (Seq.fromList "abcdefghijklmnop")
    in
        danceNTimes (Seq.fromList "abcdefghijklmnop") moves (1000000000 `mod` loop)

spin :: Seq a -> Int -> Seq a
spin seq i = uncurry (Seq.><)
           . (\(a,b) -> (b, a))
           . Seq.splitAt (Seq.length seq - i) $ seq

exchange :: Seq a -> Int -> Int -> Seq a
exchange seq i j = Seq.update i b
                 . Seq.update j a
                 $ seq
    where
        a = Seq.index seq i
        b = Seq.index seq j

partner :: (Eq a) => Seq a -> a -> a -> Seq a
partner seq a b = Seq.update i b
                . Seq.update j a
                $ seq
    where
        i = pos seq a
        j = pos seq b

pos :: (Eq a) => Seq a -> a -> Int
pos seq a = length . takeWhile (/= a) . toList $ seq

danceMove :: Seq Char -> String -> Seq Char
danceMove dancers (move:xs)
    | move == 's' = spin dancers (read xs)
    | move == 'x' = exchange dancers (read a) (read b)
    | move == 'p' = partner dancers (xs !! 0) (xs !! 2)
    where
        (a, b') = span (/= '/') xs
        b = tail b'

fullDance :: Seq Char -> [String] -> Seq Char
fullDance seq moves = foldl danceMove seq moves

danceNTimes :: Seq Char -> [String] -> Int -> Seq Char
danceNTimes seq moves n
    | n == 0 = seq
    | otherwise = danceNTimes (fullDance seq moves) moves (n-1)

findLoop :: [String] -> Seq Char -> Int
findLoop moves seq = findLoop' seq (fullDance seq moves) 1
    where
        findLoop' :: Seq Char -> Seq Char -> Int -> Int
        findLoop' seq1 seq2 n
            | seq1 == seq2 = n
            | otherwise = findLoop' seq1 (fullDance seq2 moves) (n+1)
