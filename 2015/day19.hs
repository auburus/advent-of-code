module Main where

import System.IO (readFile)
import qualified Data.Char as C
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List.Split (splitOn)
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Utils.Queue (Queue)
import qualified Utils.Queue as Q

type Change = (String, Seq String)
type Moves = Int

split :: String -> Seq String
split [] = Seq.empty
split (x:xs)
    | C.isUpper x = (x : same) Seq.<| (split remaining)
    | otherwise = error "Strange error"
    where
        (same, remaining) = span C.isLower xs

combs :: [Change] -> Seq String -> [Seq String]
combs changes seq = nub .
                    foldl (++) [] .
                    map (combs' changes seq) $ [0..(Seq.length seq -1)]

combs' :: [Change] -> Seq String -> Int -> [Seq String]
combs' changes seq i = map (\(_, x) -> prev Seq.>< x Seq.>< next) possibleChanges
    where
        (prev, current, next) = triplet i seq
        possibleChanges = filter (\x -> fst x == current) changes

triplet :: Int -> Seq a -> (Seq a, a, Seq a)
triplet i seq = (prev, current, next)
    where
        (prev, others) = Seq.splitAt i seq
        (current', next) = Seq.splitAt 1 others
        current = Seq.index current' 0

bsf :: Set (Seq String) -> [Change] -> Seq String -> Queue (Moves, Seq String) -> (Moves, Seq String)
bsf visited changes final queue
    | current == final = front
    | otherwise = bsf visited' changes final queue''
    where
        (front, queue') = Q.pop queue
        (m, current) = front
        new = filter (\x -> Set.notMember x visited) .
              combs changes $ current
        visited' = foldr Set.insert visited new
        queue'' = foldl Q.push queue' . zip (repeat (m+1)) $ new

isPrefix' :: Eq a => Seq a -> Seq a -> Bool
isPrefix' subseq seq
    | Seq.null subseq = True
    | Seq.null seq = False
    | Seq.index subseq 0 == Seq.index seq 0 =
        isPrefix' (Seq.drop 1 subseq) (Seq.drop 1 seq)
    | otherwise = False

problem1 :: [Change] -> Seq String -> Int
problem1 changes = length . combs changes

problem2 :: [Change] -> Seq String -> (Moves, Seq String)
problem2 changes final = bsf Set.empty changes final $ Q.singleton (0, Seq.fromList ["e"])

main = do
    contents <- readFile "input19.txt"
    let input = "CRnSiRnCaPTiMgYCaPTiRnFArSiThFArCaSiThSiThPBCaCaSiRnSiRnTiTiMgArPBCaPMgYPTiRnFArFArCaSiRnBPMgArPRnCaPTiRnFArCaSiThCaCaFArPBCaCaPTiTiRnFArCaSiRnSiAlYSiThRnFArArCaSiRnBFArCaCaSiRnSiThCaCaCaFYCaPTiBCaSiThCaSiThPMgArSiRnCaPBFYCaCaFArCaCaCaCaSiThCaSiRnPRnFArPBSiThPRnFArSiRnMgArCaFYFArCaSiRnSiAlArTiTiTiTiTiTiTiRnPMgArPTiTiTiBSiRnSiAlArTiTiRnPMgArCaFYBPBPTiRnSiRnMgArSiThCaFArCaSiThFArPRnFArCaSiRnTiBSiThSiRnSiAlYCaFArPRnFArSiThCaFArCaCaSiThCaCaCaSiRnPRnCaFArFYPMgArCaPBCaPBSiRnFYPBCaFArCaSiAl"
        changes = map (parse . splitOn " => ") . lines $ contents
        parse [x,y] = (x, split y)

    print . problem1 changes $ split input
    print . combs changes $ Seq.fromList ["e"]
    -- print . problem2 changes $ split input
