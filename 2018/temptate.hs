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

main :: IO ()
main = do
    input <- map parseInput . lines <$> readFile "input03.txt"

    print $ problem1 input
    print $ problem2 input

problem1 input = input

problem2 input = input

parseInput input = mymap . map unpack . split ((flip elem) " ") . pack $ input
    where
        mymap [] = ()
