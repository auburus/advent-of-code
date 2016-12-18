module Main where

data Tile = Safe | Trap deriving (Eq, Show)

nextRow :: [Tile] -> [Tile]
nextRow tiles = doNextRow (Safe : tiles ++ [Safe])
    where 
        doNextRow :: [Tile] -> [Tile]
        doNextRow (_:_:[]) = []
        doNextRow (x:s@(y:z:xs))
            | x == y && y /= z = Trap : doNextRow s
            | x /= y && y == z = Trap : doNextRow s
            | otherwise = Safe : doNextRow s

seqTiles :: [Tile] -> [[Tile]]
seqTiles tiles = tiles : seqTiles (nextRow tiles)

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
