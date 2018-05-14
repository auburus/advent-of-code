module Main where

import System.IO (readFile)
import Data.List
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M

data Type = Clear | Infected deriving (Show, Eq)
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

doPart1 :: Input -> Infections
doPart1 input =
    (\(_,_,_,a) -> a) $ doBursts 10000 ((parseInput input), (0,0), N)

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

move :: Pos -> Dir -> Pos
move (x,y) N = (x, y+1)
move (x,y) E = (x+1, y)
move (x,y) S = (x, y-1)
move (x,y) W = (x-1, y)
