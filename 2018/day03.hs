module Main where

import System.IO (readFile)
import Data.Text (split, splitOn, pack, unpack)
import Data.List
import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Applicative

main :: IO ()
main = do
    input <- map parseInput . lines <$> readFile "input03.txt"
    let fabric = foldl fillArray M.empty $ map snd input

    print $ problem1  fabric
    print $ problem2 fabric input

problem1 :: Map (Int, Int) Int -> Int
problem1 = M.size . M.filter (>1)

problem2 :: Map (Int, Int) Int -> [(Int, (Int, Int, Int, Int))] -> Int
problem2 fabric =
    head . map fst . filter (all (==1) . map ((M.!) fabric) . claimToIdx . snd)

parseInput :: String -> (Int, (Int, Int, Int, Int))
parseInput input = mymap . map unpack . split ((flip elem) "# ,:x") . pack $ input
    where
        mymap [_, id, _, x, y, _, w, h] = (read id, (read x, read y, read w, read h))
    
fillArray :: Map (Int, Int) Int -> (Int, Int, Int, Int) -> Map (Int, Int) Int
fillArray m claim = foldl ins m $ claimToIdx claim
    where
        ins map key = M.insertWith (+) key 1 map

claimToIdx :: (Int, Int, Int, Int) -> [(Int, Int)]
claimToIdx (x,y,w,h) = [ (x+i,y+j) | i <- [1..w], j <- [1..h]]
