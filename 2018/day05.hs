module Main where

import Data.Char
import System.IO (readFile)
import Data.Text (split, splitOn, pack, unpack)
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Control.Applicative

main :: IO ()
main = do
    input <- head .lines <$> readFile "input05.txt"

    let reacted = fullyReact input

    print $ length input
    print $ problem1 reacted
    print $ problem2 reacted

problem1 = length 

problem2 reacted = 
    let units = nub . map toLower $ reacted
    in 
        minimum . map ( length . fullyReact . (deleteAll reacted)) $ units
        

deleteAll :: String -> Char -> String
deleteAll [] _ = []
deleteAll (x:xs) a
    | toLower x == a = deleteAll xs a
    | otherwise = x : deleteAll xs a

        
react :: String -> String
react [] = []
react (a:[]) = [a]
react (a:b:xs)
    | isUpper a && toLower a == b = react xs
    | isLower a && toUpper a == b = react xs
    | otherwise = a : react (b:xs)

fullyReact :: String -> String
fullyReact str = 
    let reactions = iterate react str
        pairs = zip reactions (tail reactions)
    in
        fst . head . dropWhile (\(a,b) -> a/=b) $ pairs
