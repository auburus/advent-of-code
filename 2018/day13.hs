module Main where

import Data.List
import Prelude hiding (Left, Right)
import System.IO (readFile)
import Data.Array (Array)
import qualified Data.Array as A
import Control.Applicative
import Data.Tuple (swap)

data Dir = N | E | S | W deriving (Show, Eq)
data Turn = Left | Straight | Right deriving (Show, Eq, Enum)


type Wagon = ((Int, Int), Dir, Turn)

main = do
    input <- toArray <$> readFile "input13.txt"
    let wagons = getWagons input
        grid = clearGrid input wagons

    print . problem1 grid $ sort' wagons
    print . problem2 grid $ sort' wagons

    where
        toArray :: String -> Array (Int, Int) Char
        toArray s = A.listArray
            ((0,0), (flip (-) 1 . length . lines $s, length . head . lines $s)) s

problem1 grid wagons 
    | length wagons /= length (tic grid wagons) = fmap swap $ findColision grid wagons
    | otherwise = problem1 grid (tic grid wagons)
    
problem2 grid wagons
    | length wagons == 1 = swap . pos . head $ wagons
    | otherwise = problem2 grid $ tic grid wagons

getWagons :: Array (Int, Int) Char -> [Wagon]
getWagons = map (\(p, c) -> (p, toDir c, Left))
          . filter (flip elem "v^<>" . snd)
          . A.assocs 
          where
            toDir 'v' = S
            toDir '^' = N
            toDir '>' = E
            toDir '<' = W

clearGrid :: Array (Int, Int) Char -> [Wagon] -> Array (Int, Int) Char
clearGrid arr ws = arr A.// (map (\(p, d, _) -> (p, dirToSym d)) ws)
    where
        dirToSym S = '|'
        dirToSym N = '|'
        dirToSym W = '-'
        dirToSym E = '-'

findColision :: Array (Int, Int) Char -> [Wagon] -> Maybe (Int, Int)
findColision grid wagons = run wagons []
    where
        run [] _ = Nothing
        run (w:ws) ys
            | pos w' `elem` map pos (ws ++ ys) = Just (pos w')
            | otherwise = run ws (w':ys)
            where 
                w' = move grid w

sort' :: [Wagon] -> [Wagon]
sort' = sortBy (\a b -> compare (pos a) (pos b))

move :: Array (Int, Int) Char -> Wagon -> Wagon
move grid (p, d, t) = (p', d', t')
    where
        p' = forward p d
        (d', t') = newDir (d,t) (grid A.! p')
    
tic :: Array (Int, Int) Char -> [Wagon] -> [Wagon]
tic grid wagons = run wagons []
    where
        run [] ys = sort' ys
        run (w:ws) ys 
            | pos w' `elem` map pos (ws ++ ys) =
                run (filter ((/=(pos w')) . pos) ws) (filter ((/=(pos w')) . pos) ys)
            | otherwise = run ws (w':ys)
            where
                w' = move grid w

pos :: Wagon -> (Int, Int)
pos (p, _, _) = p


newDir :: (Dir, Turn) -> Char -> (Dir, Turn)
newDir (N, t) '/' = (E, t)
newDir (E, t) '/' = (N, t)
newDir (W, t) '/' = (S, t)
newDir (S, t) '/' = (W, t)

newDir (N, t) '\\' = (W, t)
newDir (W, t) '\\' = (N, t)
newDir (S, t) '\\' = (E, t)
newDir (E, t) '\\' = (S, t)

newDir (d,t) '+' = (turn d t, nextTurn t)
newDir (d,t) '-' = (d,t)
newDir (d,t) '|' = (d,t)


nextTurn :: Turn -> Turn
nextTurn Right = Left
nextTurn x = succ x

turn :: Dir -> Turn -> Dir
turn N Left = W
turn W Left = S
turn S Left = E
turn E Left = N
turn t Straight = t
turn t Right = iterate (flip turn Left) t !! 3

forward :: (Int, Int) -> Dir -> (Int, Int)
forward (x,y) N = (x-1, y)
forward (x,y) S = (x+1, y)
forward (x,y) E = (x, y+1)
forward (x,y) W = (x, y-1)

        
printGrid :: Array (Int, Int) Char -> IO ()
printGrid grid = 
    mapM_ putStr $
    map (\i -> map snd . filter ((==i) . fst . fst) . filter ((<10) . snd . fst) $ A.assocs grid) [0..100]
