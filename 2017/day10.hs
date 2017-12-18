
module Main where

import Data.List
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Data.Foldable (toList)
import Data.Char (ord)
import Data.Bits (xor)
import Numeric (showHex)

main = do
    let input = [76,1,88,148,166,217,130,0,128,254,16,2,130,71,255,229] 
        input' = "76,1,88,148,166,217,130,0,128,254,16,2,130,71,255,229"

    print . doPart1 256 $ input
    print . doPart2 256 $ input'


doPart1 :: Int -> [Int] -> Seq Int
doPart1 size lengths = hash 0 0 lengths $ S.fromList [0..(size-1)]

hash :: Int -> Int -> [Int] -> Seq Int -> Seq Int
hash _ _ [] seq = seq
hash currentPos i (l:lengths) seq =
    hash nextPos (i+1) lengths $ doHash currentPos l seq
    where
        nextPos = (currentPos + l + i) `mod` S.length seq

doHash :: Int -> Int -> Seq Int -> Seq Int
doHash i l seq = 
    let splitted = frontToBack i seq
        reversed = uncurry (S.><)
                 . (\(a,b) -> (S.reverse a, b))
                 . S.splitAt l
                 $ splitted
        unsplitted = frontToBack (S.length splitted - i) reversed
    in
        unsplitted

frontToBack :: Int -> Seq Int -> Seq Int
frontToBack i = uncurry (S.><) . (\(a,b) -> (b,a)) . S.splitAt i

doPart2 :: Int -> String -> String
doPart2 size asciiStr =
    let lengths = asciiSeq asciiStr
        repeatedLengths = foldl (++) [] . replicate 64 $ lengths
        sparseHash = hash 0 0 repeatedLengths $ S.fromList [0..(size-1)]
        denseHash = sparseToDense . toList $ sparseHash :: [Int]
    in
        foldl (++) "" 
        . map (\x -> if length x == 1 then ('0':x) else x)
        . map (\x -> showHex x "")
        $ denseHash


asciiSeq :: String -> [Int]
asciiSeq [] = [17, 31, 73, 47, 23]
asciiSeq (x:xs) = ord x : asciiSeq xs

sparseToDense :: [Int] -> [Int]
sparseToDense [] = []
sparseToDense xs = foldl xor (head first16) (tail first16) : sparseToDense rest
    where
        (first16, rest) = splitAt 16 xs
