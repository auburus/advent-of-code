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
        -- magicNumber = 10

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

bfs' :: [Pos] -> Int -> [Node] -> [Pos]
bfs' visited maxMoves queue
    | null queue = visited
    | otherwise = bfs' visited' maxMoves queue'

    where
        current = head queue
        visited' = visited ++ (map pos next)
        queue' = (tail queue) ++ next

        next = nextMoves'' current
        nextMoves' = filter (\(Node p _ ) -> p `notElem` visited) . nextMoves
        nextMoves'' = filter(\(Node _ m) -> m <= maxMoves) . nextMoves'

main = do
    let startNode = Node (Pos 1 1) 0
    
    -- print $ bfs' [] 5 [startNode]
    print $ length $ bfs' [] 50 [startNode]
