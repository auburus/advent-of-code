module Main where

import Data.List
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Foldable (toList)
import Data.List.Split (splitOn)

type State = (Int, Int, Seq Int)

main = do
    let input = 330121
        input' = [3,3,0,1,2,1]
        -- input' = [5,1,5,8,9]

    print $ problem1 input
    print $ problem2' input'


problem1 n =
    let (_, _, seq) = create (n+10) (0,1, Seq.fromList [3,7])
    in
        foldl1 (\a b -> a*10 + b) . toList . Seq.take 10 . Seq.drop n $ seq

problem2' input' =
    let (_, _, seq) = create 30000000 (0,1, Seq.fromList [3,7])

    in length . head . splitOn input' $ toList seq

problem2 input' =
    run (0,1, Seq.fromList [3,7])
    where
        l = length input'
        run (i,j, seq) 
            | input' `isInfixOf` (toList . Seq.drop (Seq.length seq - (2*l)) $ seq)
                = True
            | otherwise = run $ step (i,j, seq)

    
create :: Int -> State -> State
create n (i,j,seq)
    | (Seq.length seq) > n = (i,j,seq)
    | otherwise = create n $ step (i,j,seq)

step (i,j,seq) = ((i + rec1 + 1) `mod` len', (j + rec2 + 1) `mod` len', seq')
               where
                seq' = seq Seq.>< (numToSeq (rec1 + rec2))
                len' = Seq.length seq'
                rec1 = seq `Seq.index` i
                rec2 = seq `Seq.index` j


numToSeq :: Int -> Seq Int
numToSeq a
    | a `div` 10 > 0 = Seq.fromList [a `div` 10, a `mod` 10]
    | otherwise = Seq.singleton a


