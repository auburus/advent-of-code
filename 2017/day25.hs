module Main where

import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

data State = A | B | C | D | E | F
    deriving (Show, Eq)


type Tape = Set Int
type Cursor = Int
type Machine = (Tape, State, Cursor)


main = do
    let machine = (Set.empty, A, 0)
        n = 12481997
    -- print . checkSum . (\(x,_,_) -> x) $ (iterate next machine) !! n
    print . checkSum . (\(x,_,_) -> x) . getN n $ machine 

checkSum :: Tape -> Int
checkSum = Set.size 

getN :: Int -> Machine -> Machine
getN 0 m = m
getN n machine = getN (n-1) $! (next machine)

next :: Machine -> Machine
next (tape, state, cursor) = 
    let value = if Set.member cursor tape then 1 else 0
    in
    case (state, value) of 
        (A, 0) -> (Set.insert cursor tape, B, cursor+1)
        (A, 1) -> (Set.delete cursor tape, C, cursor-1)
        (B, 0) -> (Set.insert cursor tape, A, cursor-1)
        (B, 1) -> (tape, D, cursor+1)
        (C, 0) -> (tape, B, cursor-1)
        (C, 1) -> (Set.delete cursor tape, E, cursor-1)
        (D, 0) -> (Set.insert cursor tape, A, cursor+1)
        (D, 1) -> (Set.delete cursor tape, B, cursor+1)
        (E, 0) -> (Set.insert cursor tape, F, cursor-1)
        (E, 1) -> (tape, C, cursor-1)
        (F, 0) -> (Set.insert cursor tape, D, cursor+1)
        (F, 1) -> (tape, A, cursor+1)
        
