module Main where

import System.IO (readFile)
import Data.List.Split (splitOn)
import Data.List

data Range = Range Int Int deriving (Eq, Show)

instance Ord Range where
    (Range a _) <= (Range b _) = a <= b

toRange :: String -> Range
toRange str = Range first second
    where
        [first, second] = map read . splitOn "-" $ str

minIP :: [Range] -> Int
minIP = foldl (\m (Range min' max') -> if m < min' then m else max m (max' + 1)) 0

combine :: [Range] -> [Range]
combine [] = []
combine (x:[]) = x : []
combine (x@(Range min1 max1):y@(Range min2 max2):xs)
    | min2 > (max1+1) = x : combine (y:xs)
    | min2 < min1 = error "List not ordered"
    | otherwise = combine ((Range min1 (max max1 max2)):xs)

allowed :: [Range] -> Int
allowed [] = 0
allowed (x@(Range min' max'):xs) = min' + doRange (x:xs)
    where
        doRange :: [Range] -> Int
        doRange [] = 0
        doRange ((Range min' max'):[]) = 4294967295 - max'
        doRange ((Range min1 max1):y@(Range min2 max2):xs) =
            (min2 - max1 - 1) + doRange (y:xs)

printLines :: Show a => [a] -> IO ()
printLines [] = return ()
printLines (x:xs) = do
    print x
    printLines xs

main = do
    contents <- readFile "input20.txt"
    let contents' = "5-8\n0-2\n4-7"
        input = lines contents

    -- Problem 1
    print . minIP . sort . map toRange $ input

    -- Problem 2
    print . allowed . combine . sort . map toRange $ input
