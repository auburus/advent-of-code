module Main where

import Data.Char
import System.IO (readFile)
import Data.Text (split, splitOn, pack, unpack)
import Data.List
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.Array (Array)
import qualified Data.Array as A
import Control.Applicative


data Node = Node { children :: [Node]
                 , metadata :: [Int]
                 } deriving (Show)
main :: IO ()
main = do
    input <- head . map parseInput . lines <$> readFile "input08.txt"

    print $ problem1 input
    print $ problem2 input

n = Node [(Node [] [10, 11, 12]), (Node [(Node [] [99])] [2])] [1,1,2]

problem1 input = sum . metadatas . parse $ input

problem2 input = value . parse $ input

parseInput :: String -> [Int]
parseInput input = mymap . map unpack . split ((flip elem) " ") . pack $ input
    where
        mymap = map read

metadatas :: Node -> [Int]
metadatas n = (metadata n) ++ (concat . map metadatas $ children n)


parse :: [Int] -> Node
parse xs = fst $ parseNode xs

parseNode :: [Int] -> (Node, [Int])
parseNode (c:m:xs) =
    let (childs, xs') = parseChildren c [] xs
        (meta, xs'') = parseMeta m xs'
    in
        (Node {children=childs, metadata=meta}, xs'')

parseChildren :: Int -> [Node] -> [Int] -> ([Node], [Int])
parseChildren 0 childs xs = (reverse childs, xs)
parseChildren n childs xs = 
    let (c, xs') = parseNode xs
    in
        parseChildren (n-1) (c:childs) xs'

parseMeta :: Int -> [Int] -> ([Int], [Int])
parseMeta n xs = splitAt n xs

value :: Node -> Int
value node
    | null . children $ node = sum . metadata $ node
    | otherwise = sum . map (maybe 0 value . index' (children node)) $ metadata node

index' :: [Node] -> Int -> Maybe Node
index' nodes i
    | i > 0 && i <= length nodes = Just (nodes !! (i-1))
    | otherwise = Nothing
