module Main where

import System.IO (readFile)
import Data.List
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M

data Type = Clear | Infected | Weakened | Flagged deriving (Show, Eq)
type Pos = (Integer, Integer)
type Grid = Map Pos Type
data Dir = N | S | E | W deriving (Show, Eq)
type Infections = Int
type Input = [String]

grid = S.insert (1,1) . S.insert (-1, 0) $ S.empty
start = (0,0) :: Pos

main = do
    contents <- readFile "input22.txt"
    let input = lines $ contents

    print . doPart1 $ input
    print . doPart2 $ input


doPart1 :: Input -> Infections
doPart1 input =
    (\(_,_,_,a) -> a) $ doBursts 10000 ((parseInput input), (0,0), N)

doPart2 :: Input -> Infections
doPart2 input =
    (\(_,_,_,a) -> a) $ doBursts2 10000000 ((parseInput input), (0,0), N)

parseInput :: Input -> Grid
parseInput input = foldl (\map (pos, _) -> M.insert pos Infected map) M.empty
                 . filter ((=='#') . snd)
                 . zip [(i, j) | j <- vy, i <- vx]
                 . foldl (++) []
                 $ input
    where
        bound = fromIntegral
              $ (div) (length input) 2
                
        vx = [(-bound)..bound] :: [Integer]
        vy = [bound, (bound-1)..(-bound)] :: [Integer]

doBursts :: Int -> (Grid, Pos, Dir) -> (Grid, Pos, Dir, Infections)
doBursts n (grid, pos, dir) = iterate burst (grid, pos, dir, 0) !! n

doBursts2 :: Int -> (Grid, Pos, Dir) -> (Grid, Pos, Dir, Infections)
doBursts2 n (grid, pos, dir) = iterate burst2 (grid, pos, dir, 0) !! n

burst2 :: (Grid, Pos, Dir, Infections) -> (Grid, Pos, Dir, Infections)
burst2 (grid, pos, dir, n) =
    let cell = M.findWithDefault Clear pos grid
    in case cell of
        Clear -> ( M.insert pos Weakened grid
                 , move pos (turnLeft dir)
                 , turnLeft dir
                 , n)

        Infected -> ( M.insert pos Flagged grid
                    , move pos (turnRight dir)
                    , turnRight dir
                    , n)

        Flagged -> ( M.insert pos Clear grid
                   , move pos (reverseDir dir)
                   , reverseDir dir
                   , n)

        Weakened -> ( M.insert pos Infected grid
                    , move pos dir
                    , dir
                    , n+1 )

burst :: (Grid, Pos, Dir, Infections) -> (Grid, Pos, Dir, Infections)
burst (grid, pos, dir, n) =
    let cell = M.findWithDefault Clear pos grid
    in case cell of
        Infected -> ( M.insert pos Clear grid
                    , move pos (turnRight dir)
                    , turnRight dir
                    , n)
        Clear -> ( M.insert pos Infected grid
                 , move pos (turnLeft  dir)
                 , turnLeft  dir
                 , n+1)

turnRight :: Dir -> Dir
turnRight N = E
turnRight E = S
turnRight S = W
turnRight W = N

turnLeft :: Dir -> Dir
turnLeft = turnRight . turnRight . turnRight

reverseDir :: Dir -> Dir
reverseDir = turnRight . turnRight

move :: Pos -> Dir -> Pos
move (x,y) N = (x, y+1)
move (x,y) E = (x+1, y)
move (x,y) S = (x, y-1)
move (x,y) W = (x-1, y)
