module Main where

import Data.List
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Foldable (toList)

main = do
    let input = 366

    print . doPart1 $ input
    print . doPart2 $ input

doPart1 :: Int -> Int
doPart1 input = 
    let seq = genSeq input 2017
    in
        head . toList . rotate ((indexOf 2017 seq) + 1) $ seq

genSeq :: Int -> Int -> Seq Int
genSeq input n = foldl (nextSeq input) (Seq.fromList [0]) [1,2..n]

doPart2 :: Int -> Int
doPart2 input = nextToZero input 0 [1..50000000] 0

addNum :: Seq Int -> Int -> Seq Int
addNum seq num = num Seq.<| seq

rotate :: Int -> Seq a -> Seq a
rotate i seq = uncurry (\front back -> back Seq.>< front)
    $ Seq.splitAt (i `mod` Seq.length seq) seq

nextSeq :: Int -> Seq Int -> Int -> Seq Int
nextSeq step seq num = addNum (rotate (step + 1) seq ) num

indexOf :: (Eq a ) => a -> Seq a -> Int
indexOf item seq = length . takeWhile (/= item) . toList $ seq 

nextPos :: Int -> Int -> Int -> Int
nextPos step current n = (step + current ) `mod` n

nextToZero :: Int -> Int -> [Int] -> Int -> Int
nextToZero step pos [] n = n
nextToZero step pos (x:xs) n
    | next == 0 = nextToZero step (next+1) xs x
    | otherwise = nextToZero step (next+1) xs n
    where
        next = nextPos step pos x
