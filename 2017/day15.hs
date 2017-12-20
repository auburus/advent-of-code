module Main where

import Data.List
import Numeric (showIntAtBase)
import Data.Char (intToDigit)

main = do
    let input = (679, 771)

    -- print . doPart1 $ input
    print . doPart2 $ input


doPart1 :: (Integer, Integer) -> Integer
doPart1 (a, b) = sum
               . take 40000000
               . map (\c -> if c then 1 else 0)
               $ judge (generateA a) (generateB b)

doPart2 :: (Integer, Integer) -> Integer
doPart2 (a, b) = sum
               . take 5000000
               . map (\c -> if c then 1 else 0)
               $ judge (generateA' a) (generateB' b)

generate :: Integer -> Integer -> Integer
generate factor prev = (prev * factor) `mod` 2147483647

generateA :: Integer -> [String]
generateA a = tail
            . map (\x -> showIntAtBase 2 intToDigit x "" )
            $ iterate (generate 16807) a

generateA' :: Integer -> [String]
generateA' a =
             map (\x -> showIntAtBase 2 intToDigit x "" )
             . filter (\x -> x `mod` 4 == 0)
             . tail
             $ iterate (generate 16807) a

generateB :: Integer -> [String]
generateB b = tail
            . map (\x -> showIntAtBase 2 intToDigit x "" )
            $ iterate (generate 48271) b

generateB' :: Integer -> [String]
generateB' b =
             map (\x -> showIntAtBase 2 intToDigit x "" )
             . filter (\x -> x `mod` 8 == 0)
             . tail
             $ iterate (generate 48271) b

last16 :: String -> String
last16 = reverse . take 16 . reverse

judge :: [String] -> [String] -> [Bool]
judge a b = zipWith (==) (map last16 a) (map last16 b)
