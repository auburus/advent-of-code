module Main where

import System.IO
import Data.List
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe

type Program = String
type Weights = Map Program Int

main = do
    contents <- readFile "input07.txt"

    let input = lines contents

    print . doPart1 $ input
    print . doPart2 $ input

doPart1 :: [String] -> Program
doPart1 input =
    let m = buildBottomUpMap . map parseInput $ input

    in
        findTrueBottom m

doPart2 :: [String] -> (Int, [(Program, Int)])
doPart2 input =
    let weights = buildWeights input
        input' = map parseInput input
        childs = buildChildMap input'
        rootElement = doPart1 input
        accum = accumWeights weights childs $ rootElement
        elem = "ptshtrn"
    in
        (weights M.! elem, childWeights accum childs elem)
        

parseInput :: String -> (Program, [Program])
parseInput input
    | isInfixOf "->" input = (program, programs)
    | otherwise = (program, [])
    where
        program = takeWhile (/= ' ') input
        rem = drop 2 . dropWhile (/= '>') $ input
        programs = splitOn ", " rem 

buildBottomUpMap :: [(Program, [Program])] -> Map Program Program
buildBottomUpMap [] = M.empty
buildBottomUpMap ((program, programs):xs) = 
    let buildMap' :: Map Program Program -> [Program] -> Map Program Program
        buildMap' map [] = map
        buildMap' map (x:xs) = M.insert x program $ buildMap' map xs
    in
        buildMap' (buildBottomUpMap xs) programs

findTrueBottom :: Map Program Program -> Program
findTrueBottom map = findTrueBottom' map (head . M.keys $ map)

findTrueBottom' :: Map Program Program -> Program -> Program
findTrueBottom' map program
    | M.member program map = findTrueBottom' map ( map M.! program)
    | otherwise = program

buildChildMap :: [(Program, [Program])] -> Map Program [Program]
buildChildMap [] = M.empty
buildChildMap ((program, programs):xs) = M.insert program programs $ buildChildMap xs

buildWeights :: [String] -> Weights
buildWeights [] = M.empty
buildWeights (x:xs) = M.insert program weight $ buildWeights xs
    where
        program = head . words $ x
        weight = read . takeWhile (/= ')') . tail . dropWhile (/='(') $ x


accumWeights :: Weights -> Map Program [Program] -> Program -> Weights
accumWeights weights childs p =
    M.insert p (programWeight + childsWeight) $ (childsMap (childs M.! p))
    where
        childsMap :: [Program] -> Weights
        childsMap [] = M.empty
        childsMap xs = M.unions . map (accumWeights weights childs) $ xs
        
        programWeight = weights M.! p
        childsWeight = sum . map ((M.!) (childsMap $ childs M.! p)) $ childs M.! p

elemUnbalanced :: Weights -> Map Program [Program] -> Program -> Bool
elemUnbalanced weights childs p = all (== head c) $ tail c
    where
        c = map ((M.!) weights) $ childs M.! p

childWeights :: Weights -> Map Program [Program] -> Program -> [(Program, Int)]
childWeights weights childs p =
    zip (childs M.! p) . map ((M.!) weights) $ (childs M.! p)

-- findWrongProgram :: Weights -> Map Program [Program] -> Int -> Program -> (Program, Int)
-- findWrongProgram weights childs expectedWeight p =
--     case differentChild of
--         Nothing -> (p, expectedWeight)
--         Just (child, w) -> findWrongProgram weights childs w child
--     where
--         differentChild =
--             filter (\xs -> length xs == 1) .
--             group .
--             sort .
--             map (\x -> zip x . weights M.! x)$
--             childs M.! p
