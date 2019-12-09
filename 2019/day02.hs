module Day02 where

import System.IO
import Data.List.Split (splitOn)
import Data.Array.IArray (Array)
import qualified Data.Array.IArray as A


type OpCode = Integer
type Index = Integer
type Instructions = Array Index OpCode

main = do
    input <- splitOn "," . head . lines <$> readFile "input02.txt"
    let input' = A.listArray (0, toInteger $ length input - 1) $ map read input :: Instructions
    let input'' = A.listArray(0, 11) $ [1,9,10,3,2,3,11,0,99,30,40,50] :: Instructions

    print . problem1 $ input'
    print . problem2 $ input'

problem1 ins = getOutput $ prepareIns ins 12 2

problem2 :: Instructions -> Integer
problem2 ins = (\(n, v, _) -> 100 * n + v)
             . head
             . filter (\(_, _, x) -> x == 19690720)
             . map (\(n,v) -> (n, v, getOutput $ prepareIns ins n v))
             $ [(noun, verb) | noun <- [0..99], verb <- [0..99]]
    
getOutput :: Instructions -> Integer
getOutput ins = flip (A.!) 0
              . fst
              . last 
              . takeWhile (\(_, a) -> a /= -1)
              $ iterate run (ins, 0)

prepareIns :: Instructions -> Integer -> Integer -> Instructions
prepareIns ins noun verb = ins A.// [(1, noun), (2, verb)]

run :: (Instructions, Index) -> (Instructions, Index)
run (ins, i)
    | op == 99 = (ins, -1)
    | op == 1 =
        (ins A.// [(ins A.! (i+3), ((A.!) ins . (A.!) ins $ (i+1))
                                 + ((A.!) ins . (A.!) ins $ (i+2)))], i+4)
    | op == 2 =
        (ins A.// [(ins A.! (i+3), ((A.!) ins . (A.!) ins $ (i+1))
                                 * ((A.!) ins . (A.!) ins $ (i+2)))], i+4)
    
    | otherwise = error "hasdfasdfa"
        
    where
        op = ins A.! i
    
