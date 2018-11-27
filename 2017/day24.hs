module Main where

import System.IO
import Data.List
import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as S

type Port = Int
type Component = (Port, Port)
type Bridge = [Component]

main = do
    -- contents <- readFile "input.txt"
    contents <- readFile "input24.txt"

    let components = map parseInput . lines $ contents
        bridges = allBridges 0 $ components
        len = maxLength bridges

    print . maximum . map strength $ bridges
    print . maximum . map strength . filter (\x -> length x == len)
        $ bridges
    

maxLength :: [Bridge] -> Int
maxLength = maximum . map length

buildSet :: [Component] -> Set Component
buildSet [] = S.empty
buildSet (x:xs) = S.insert x $ buildSet xs


parseInput :: String -> Component
parseInput x = (head splitted, last splitted) 
    where
        splitted = map read . splitOn "/" $ x

strength :: Bridge -> Int
strength [] = 0
strength ((x,y):xs) = x + y + strength xs


possibleConnections :: Port -> [Component] -> [Component]
possibleConnections port = filter (\(a,b) -> a == port || b == port)

allBridges :: Port -> [Component] -> [Bridge]
allBridges port components
    | null possible = [[]]
    | otherwise = foldl (++) []
        . map (\x -> zipWith (:) (repeat x) (allBridges (newPort port x) (delete' x components)))
        $ possible

    where
        possible = possibleConnections port components

        delete' :: Eq a => (a,a) -> [(a,a)] -> [(a,a)]
        delete' = deleteBy (\(a,b) (x,y) -> (a==x && b==y) || (a==y && b==x))

        newPort :: Port -> Component -> Port
        newPort port (a,b) = if port == a then b else a

