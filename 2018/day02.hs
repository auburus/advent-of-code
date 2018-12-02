module Main where

import System.IO (readFile)
import Data.List
import Control.Applicative

main = do
    input <- lines <$> readFile "input02.txt"

    print . problem1 $ input
    print . problem2 $ input


appearsNtimes :: Int -> String -> Bool
appearsNtimes n = any ((==n) . length) . group . sort 

-- Assume all input is the same length
similarStr :: String -> String -> Bool
similarStr str1 = (==(length str1 -1)) . length . filter id . zipWith (==) str1

problem1 :: [String] -> Int
problem1 xs = (length . filter (appearsNtimes 2) $ xs) * (length . filter (appearsNtimes 3) $ xs)

problem2 :: [String] -> String
problem2 (x:xs) =
    case filter (similarStr x) xs of
        (a:[]) -> map fst . filter (uncurry (==)) $ zip x a
        _ -> problem2 xs


    
