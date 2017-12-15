module Main where

import System.IO
import Data.List
import Data.Map (Map)
import qualified Data.Map as M

type Registers = Map String Int

main = do
    contents <- readFile "input08.txt"
    let input = lines $ contents 

    print . doPart1 $ input

doPart1 :: [String] -> (String, Int)
doPart1 input = maxRegister . foldl execInstruction M.empty $ input

execInstruction :: Registers -> String -> Registers
execInstruction reg instruction
    | cond $ M.findWithDefault 0 (ins' !! 4) reg = M.insert (ins' !! 0) newVal reg
    | otherwise = reg

    where
        ins' = words instruction
        op = parseOp (ins' !! 1) (ins' !! 2)
        cond = parseCondition (ins' !! 5) (ins' !! 6)
        newVal = op $ M.findWithDefault 0 (ins' !! 0) reg

maxRegister :: Registers -> (String, Int)
maxRegister reg = M.foldlWithKey
    (\(maxId, maxVal) id val -> if val > maxVal
                                then (id, val)
                                else (maxId, maxVal) )
    (head . M.assocs $ reg)
    reg

parseOp :: String -> String -> Int -> Int
parseOp op val
    | op == "inc" = (\a -> a + val')
    | op == "dec" = (\a -> a - val')

    where
        val' = read val :: Int

parseCondition :: String -> String -> Int -> Bool
parseCondition operator val
    | operator == ">" = (> val')
    | operator == "<" = (< val')
    | operator == ">=" = (>= val')
    | operator == "<=" = (<= val')
    | operator == "==" = (== val')
    | operator == "!=" = (/= val')

    where
        val' = read val :: Int
