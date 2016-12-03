
module Main where

input="L3,R1,L4,L1,L2,R4,L3,L3,R2,R3,L5,R1,R3,L4,L1,L2,R2,R1,L4,L4,R2,L5,R3,R2,R1,L1,L2,R2,R2,L1,L1,R2,R1,L3,L5,R4,L3,R3,R3,L5,L190,L4,R4,R51,L4,R5,R5,R2,L1,L3,R1,R4,L3,R1,R3,L5,L4,R2,R5,R2,L1,L5,L1,L1,R78,L3,R2,L3,R5,L2,R2,R4,L1,L4,R1,R185,R3,L4,L1,L1,L3,R4,L4,L1,R5,L5,L1,R5,L1,R2,L5,L2,R4,R3,L2,R3,R1,L3,L5,L4,R3,L2,L4,L5,L4,R1,L1,R5,L2,R4,R2,R3,L1,L1,L4,L3,R4,L3,L5,R2,L5,L1,L1,R2,R3,L5,L3,L2,L1,L4,R4,R4,L2,R3,R1,L2,R1,L2,L2,R3,R3,L1,R4,L5,L3,R4,R4,R1,L2,L5,L3,R1,R4,L2,R5,R4,R2,L5,L3,R4,R1,L1,R5,L3,R1,R5,L2,R1,L5,L2,R2,L2,L3,R3,R3,R1"


-- Hauria d'estar a un prelude o algo... pero ja va be programar-la
split :: Char -> String -> [String]
split char [] = []
split char string =
  let (slice, remaining) = span (/= char) string
  in
    slice : split char (tail' remaining)

tail' :: [a] -> [a]
tail' [] = []
tail' xs = tail xs

data Direction = North | West | South | East deriving (Show, Eq)
data Turn = Left | Right deriving (Show, Eq)

type Coord = (Int, Int)
type Position = (Coord, Direction)
type Move = (Turn, Int)

inputToMove :: String -> Move
inputToMove (dir:number) =
  case dir of
    'L' -> (Main.Left, read number)
    'R' -> (Main.Right, read number)

instructions :: [Move]
instructions = map inputToMove (split ',' input)

startPos :: Position
startPos = ((0,0), North)

changeDir :: Direction -> Turn -> Direction
changeDir dir turn =
  case dir of
    North -> if turn == Main.Left then West  else East
    West  -> if turn == Main.Left then South else North
    South -> if turn == Main.Left then East else West
    East  -> if turn == Main.Left then North else South

forward :: Position -> Int -> Position
forward ((x, y), dir) steps =
  case dir of
    North -> ((x, y+steps), dir)
    West  -> ((x-steps, y), dir)
    South -> ((x, y-steps), dir)
    East  -> ((x+steps, y), dir)
    
move :: Position -> Move -> Position
move pos@(coord, direction) (turn, steps) =
  let 
    orientedPos = (coord, changeDir direction turn)
  in
    forward orientedPos steps

path :: Position -> Int -> [Position]
path pos 0 = [pos]
path pos steps = pos : path (forward pos 1) (steps - 1)

move' :: Position -> Move -> [Position]
move' pos@(coord, direction) (turn, steps) =
  let
    orientedPos = (coord, changeDir direction turn)
  in
    path orientedPos steps

finalPositions :: [Position]
finalPositions = startPos : zipWith move finalPositions instructions

finalCoords :: [Coord]
finalCoords = map fst finalPositions

fillGaps :: [Coord] -> [Coord]
fillGaps [] = []
fillGaps [pos] = pos : fillGaps []
fillGaps ((x1,y1):s@((x2, y2):xs))
    | (x1, y1) == (x2, y2) =
        fillGaps ((x1, y1):xs)
    | x1 == x2 =
          if y1 < y2 then
            (x1, y1) : fillGaps ((x1, y1 + 1) : s)
          else
            (x1, y1) : (fillGaps ((x1, y1 - 1) : s))
    | y1 == y2 =
          if x1 < x2 then
            (x1, y1) : (fillGaps ((x1 + 1, y1) : s))
          else
            (x1, y1) : (fillGaps ((x1 - 1, y1) : s))

allCoords :: [Coord]
allCoords = fillGaps finalCoords

repeated :: [Coord] -> [Coord]
repeated [] = []
repeated (x:xs)
  | elem x xs = x : repeated xs
  | otherwise = repeated xs

main = do
  let
    result1 = foldl move startPos instructions
    result = repeated allCoords

  print result

