module Main where

data Pos = Pos { x :: Int, y :: Int } deriving (Show, Eq)
data Node = Node { pos :: Pos, moves :: Int} deriving (Show)

toBinary :: Int -> [Int]
toBinary a
    | a == 0 = [0]
    | a == 1 = [1]
    | otherwise =
        toBinary (a `div` 2) ++ [a `mod` 2]

isOpen :: Pos -> Bool
isOpen (Pos x y) =
    let multiplied = x*x + 3*x + 2*x*y + y + y*y
        magicNumber = 1362

    in
        (even . sum . toBinary) (multiplied + magicNumber)

isWall :: Pos -> Bool
isWall = not . isOpen

nextMoves :: Node -> [Node]
nextMoves node =
    (map (\p -> Node p (moves node + 1)) . filter isOpen ) next''
    where
        current = pos node
        next' = [ current { x = (x current - 1) }
                , current { x = (x current + 1) }
                , current { y = (y current - 1) }
                , current { y = (y current + 1) }
                ]
        next'' = filter (\(Pos x y) -> x >= 0 && y >= 0) next'

bfs :: Pos -> [Node] -> Int
bfs final queue
    | final == pos current = moves current
    | otherwise = bfs final queue'
    
    where
        current = head queue
        queue' = (tail queue) ++ (nextMoves current)

-- Same as above, but with memory
-- memory is a list... it should be a Map, but as long as it works...
bfs' :: [Pos] -> Pos -> [Node] -> Int
bfs' visited final queue
    | final == pos current = moves current
    | otherwise = bfs' visited' final queue'

    where
        current = head queue
        visited' = (pos current) : visited
        queue' = (tail queue) ++ (nextMoves' current)
        nextMoves' = filter (\(Node p _ ) -> p `notElem` visited) . nextMoves

main = do
    let row = map (\x -> (x,0)) [0..9]
        startNode = Node (Pos 1 1) 0
    
    print $ bfs' [] (Pos 31 39) [startNode]
