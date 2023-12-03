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
import Data.List.Split (splitOn)

type Pos = (Int, Int)
type Connections = Set Char


main :: IO ()
main = do
    input <- init . tail . head . lines <$> readFile "input20.txt"
    let input2 = "ENWWW(NEEE|SSE(EE|N))S"
        input3 = "SE(E|NW)N"

    print $ problem1 input3

problem1 input = M.toList . fst $ run M.empty (0,0) input

flatten :: String -> [String]
flatten [] = [[]]
flatten xs = map ((++) h) ["h", "o"]
    where
        (h,t) = span (/='(') xs
        (inner, end) = takeBlock [] t 0

takeBlock :: String -> String -> Int -> (String, String)
takeBlock xs ('(':ys) 0 = takeBlock (xs) ys 1
takeBlock xs (')':ys) 1 = (reverse xs, ys)
takeBlock xs ('(':ys) i = takeBlock ('(':xs) ys (i+1)
takeBlock xs (')':ys) i = takeBlock (')':xs) ys (i-1)
takeBlock xs ( c :ys) i = takeBlock ( c :xs) ys i



run :: Map Pos Connections -> Pos -> String -> (Map Pos Connections, String)
run m pos [] = (m, [])
run m pos ('(':xs) = (m, xs)
run m pos ('|':xs) = (m, xs)
run m pos (')':xs) = (m, xs)
run m pos (c:xs) = run m' pos' xs
    where
        m' = M.insertWith S.union pos  (S.singleton c)
           . M.insertWith S.union pos' (S.singleton $ oppositeDoor c)
           $ m

        pos' = move pos c

        oppositeDoor 'N' = 'S'
        oppositeDoor 'S' = 'N'
        oppositeDoor 'W' = 'E'
        oppositeDoor 'E' = 'W'


move :: Pos -> Char -> Pos
move p c = add p $ toVector c
    where
        toVector 'N' = (0, -1)
        toVector 'E' = (1,  0)
        toVector 'S' = (0,  1)
        toVector 'W' = (-1, 0)

add :: Pos -> Pos -> Pos
add (a,b) (c,d) = (a+c, b+d)

