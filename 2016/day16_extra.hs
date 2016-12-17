module Main where

import Data.List
import qualified Data.Char as C

type Data = [Bool]

flip' :: Data -> Data
flip' = map not

expand :: Data -> Data
expand dat = dat ++ doExpand dat
    where
        doExpand :: Data -> Data
        doExpand dat' = 
            let extra = False : ((flip' . reverse) dat')
            in 
                extra ++ doExpand (dat' ++ extra)

merge :: [a] -> [a] -> [a]
merge (x:xs) (y:ys) = x : y : merge xs ys

seq1 :: Data -> [Data]
seq1 dat = merge (repeat dat) (repeat ((flip' . reverse) dat))

seq2 :: Data
seq2 = expand [False]

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
            | x == y = True : reduced xs
            | otherwise = False : reduced xs

-- Trying to be smarter, but it's actually slower than the original one...
checksum' :: Data -> Data
checksum' dat = doChecksum (steps dat) dat
    where
        steps = foldl (\b _ -> b * 2) 1 .
                takeWhile id .
                map (\x -> (x `mod` 2) == 0) .
                iterate (\x -> x `div` 2) .
                length

        reduced :: Data -> Bool
        reduced = even . foldl (\b a -> if a then b+1 else b) 0

        doChecksum :: Int -> Data -> Data
        doChecksum _ [] = []
        doChecksum times dat = reduced first : doChecksum times remaining
            where (first, remaining) = splitAt times dat


toStr :: Data -> String
toStr = map (\b -> if b then '1' else '0')

main = do
    let input = map (\c -> if c == '1' then True else False) "11101000110010100"
        len = 272
        len' = 35651584

    -- Option 1. Do it hard!
    -- print $ (checksum . take len' . expand) input

    -- Option 2. Do it smart!
    print . toStr . checksum . take len' . merge' seq2 $ seq1 input
    -- print . toStr . checksum' . take len' . merge' seq2 $ seq1 input
