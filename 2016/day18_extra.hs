module Main where

import Data.List (tails)

data Tile = Safe | Trap deriving (Eq, Show)

nextRow :: [Tile] -> [Tile]
nextRow tiles = [compute a b | a:_:b:_ <- tails (Safe : tiles ++ [Safe])]

compute :: Tile -> Tile -> Tile
compute a b = if a == b then Safe else Trap

seqTiles :: [Tile] -> [[Tile]]
seqTiles = iterate nextRow

countSafe :: [Tile] -> Int
countSafe = foldl (\b a -> if a == Safe then b+1 else b) 0

printRows :: Show a => [a] -> IO ()
printRows [] = return ()
printRows (x:xs) = do
    print x
    printRows xs

main = do
    let input = "......^.^^.....^^^^^^^^^...^.^..^^.^^^..^.^..^.^^^.^^^^..^^.^.^.....^^^^^..^..^^^..^^.^.^..^^..^^^.."
        input' = ".^^.^.^^^^"

        startTile = map (\x -> if x == '.' then Safe else Trap) input

    -- printRows . take 40 . seqTiles $ startTile
    print . sum . map countSafe . take 400000 . seqTiles $ startTile
