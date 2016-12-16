module Main where

import Data.List
import qualified Data.Char as C

type Data = [Int]

{-
expand :: Int -> Data -> Data
expand len dat
    | length dat >= len = take len dat
    | otherwise = expand len (dat ++ [0] ++ (flip . reverse) dat)
-}

flip' :: Data -> Data
flip' = map (\x -> (x + 1) `mod` 2)

expand :: Data -> Data
expand dat = dat ++ doExpand dat
    where
        doExpand :: Data -> Data
        doExpand dat' = 
            let extra = 0 : ((flip' . reverse) dat')
            in 
                extra ++ doExpand (dat' ++ extra)

merge :: [a] -> [a] -> [a]
merge (x:xs) (y:ys) = x : y : merge xs ys

seq1 :: Data -> [Data]
seq1 dat = merge (repeat dat) (repeat ((flip' . reverse) dat))

seq2 :: Data
seq2 = expand [0]

merge' :: Data -> [Data] -> Data
merge' xs (y:ys) = y ++ doMerge xs ys
    where
        doMerge :: Data -> [Data] -> Data
        doMerge (x:xs) (y:ys) = (x : y) ++ (doMerge xs ys)

checksum :: Data -> Data
checksum dat
    | (odd . length) dat = dat
    | otherwise = checksum (reduced dat)
    where
        reduced [] = []
        reduced (x:y:xs)
            | x == y = 1 : reduced xs
            | otherwise = 0 : reduced xs

main = do
    let input = map C.digitToInt "11101000110010100"
        len = 272
        len' = 35651584

    -- Option 1. Do it hard!
    -- print $ (checksum . take len' . expand) input

    -- Option 2. Do it smart!
    print $ (checksum . take len' . merge' seq2) (seq1 input)
