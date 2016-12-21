module Main where

import Prelude hiding (reverse)
import System.IO (readFile)
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import qualified Data.List as List

data Instruction = SwapPos Int Int
                 | SwapVal Char Char
                 | RotateR Int
                 | RotateL Int
                 | RotateSpecial Char
                 | Reverse Int Int
                 | Move Int Int
                 | UnrotateSpecial Char
                 deriving (Show)

delete :: Seq a -> Int -> Seq a
delete seq i = part1 S.>< S.drop 1 part2
    where
        (part1, part2) = S.splitAt i seq

insert :: Seq a -> Int -> a -> Seq a
insert seq i a = (part1 S.|> a) S.>< part2
    where
        (part1, part2) = S.splitAt i seq

find :: Eq a => Seq a -> a -> Int
find seq a =
    let result = S.elemIndexL a seq
    in case result of
        Nothing -> error "Invalid instruction"
        Just pos -> pos

swapPos :: Seq a -> Int -> Int -> Seq a
swapPos seq i j = S.update i b $ S.update j a seq
    where
        a = S.index seq i
        b = S.index seq j

swapVal :: Eq a => Seq a -> a -> a -> Seq a
swapVal seq a b = swapPos seq (find seq a) (find seq b)

rotateL :: Seq a -> Int -> Seq a
rotateL seq i = part2 S.>< part1
    where
        (part1, part2) = S.splitAt (i `mod` S.length seq) seq

rotateR :: Seq a -> Int -> Seq a
rotateR seq i = rotateL seq (S.length seq - i)

rotateSpecial :: Eq a => Seq a -> a -> Seq a
rotateSpecial seq a =
    let i = find seq a
        i' = if i >= 4 then i+2 else i+1
    in
        rotateR seq i'

move :: Seq a -> Int -> Int -> Seq a
move seq i j = insert (delete seq i) j val
    where
        val = S.index seq i 
        deleted = delete seq

reverse :: Seq a -> Int -> Int -> Seq a
reverse seq i j = head S.>< (S.reverse middle) S.>< tail
    where
        (head, other) = S.splitAt i seq
        (middle, tail) = S.splitAt (j-i+1) other

unrotateSpecial :: Eq a => Seq a -> a -> Seq a
unrotateSpecial seq a 
    | even i' = rotateL seq (5 + (i' `div` 2))
    | odd i' = rotateL seq ((i' + 1) `div` 2)
    where
        i = find seq a
        i' = if i == 0 then 8 else i
        

exec :: Seq Char -> [Instruction] -> Seq Char
exec seq [] = seq
exec seq (x:xs) = exec updatedSeq xs
    where
    updatedSeq = case x of
            SwapPos i j -> swapPos seq i j
            SwapVal a b -> swapVal seq a b
            RotateR i -> rotateR seq i
            RotateL i -> rotateL seq i
            RotateSpecial a -> rotateSpecial seq a
            Reverse i j -> reverse seq i j
            Move i j -> move seq i j
            UnrotateSpecial a -> unrotateSpecial seq a
            

mapInstr :: String -> Instruction
mapInstr str
    | fstWord == "swap" && sndWord == "position" =
        SwapPos (read (splitted !! 2)) (read (splitted !! 5))
    | fstWord == "swap" && sndWord == "letter" =
        SwapVal (head (splitted !! 2)) (head (splitted !! 5))
    | fstWord == "rotate" && sndWord == "left" =
        RotateL (read (splitted !! 2))
    | fstWord == "rotate" && sndWord == "right" =
        RotateR (read (splitted !! 2))
    | fstWord == "rotate" && sndWord == "based" =
        RotateSpecial (head (splitted !! 6))
    | fstWord == "reverse" =
        Reverse (read (splitted !! 2)) (read (splitted !! 4))
    | fstWord == "move" =
        Move (read (splitted !! 2)) (read (splitted !! 5))
    where
        splitted = words str
        fstWord = head splitted
        sndWord = splitted !! 1

contrary :: Instruction -> Instruction
contrary instr =
    case instr of
        RotateR i -> RotateL i
        RotateL i -> RotateR i
        Move i j -> Move j i
        RotateSpecial a -> UnrotateSpecial a
        a -> a

problem1 :: Seq Char -> [String] -> Seq Char
problem1 word input = exec word . map mapInstr $ input

problem2 :: Seq Char -> [String] -> Seq Char
problem2 word input = exec word . List.reverse . map (contrary. mapInstr) $ input

-- As suggested by reddit, bruteforce it
problem2' :: Seq Char -> [String] -> [(Seq Char, Seq Char)]
problem2' word input =
    let allWords :: [Seq Char]
        allWords = map S.fromList . List.permutations $ "abcdefgh"
        pairs = zip allWords $ map (\x -> problem1 x input) allWords
    in
        filter (\(_,a) -> a == word) pairs

main = do
    contents <- readFile "input21.txt"
    let word = S.fromList "abcdefgh"
        input = lines contents

    -- Problem 1
    print . problem1 (S.fromList "abcdefgh") $ input

    -- Problem 2
    print . problem2 (S.fromList "fbgdceah") $ input
    print . problem2' (S.fromList "fbgdceah") $ input
