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

    print $ (checksum . take len' . expand) input
