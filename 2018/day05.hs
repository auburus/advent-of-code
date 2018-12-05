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


-- Changed the code to use a stack, works way better
fullyReact :: String -> String
fullyReact = foldl r [] 
    where
        r (a:xs) b
            | isUpper a && toLower a == b = xs
            | isLower a && toUpper a == b = xs
            | otherwise = b:a:xs
        r [] b = [b]
