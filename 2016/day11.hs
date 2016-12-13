module Main where

import Data.List

data Item = Generator Char | Microchip Char deriving (Show, Eq)

data State = State { elevator :: Int
                   , floor1 :: [Item]
                   , floor2 :: [Item]
                   , floor3 :: [Item]
                   , floor4 :: [Item]
                   } deriving (Show)

getFloor :: State -> Int -> [Item]
getFloor state i
    | i == 1 = floor1 state
    | i == 2 = floor2 state
    | i == 3 = floor3 state
    | i == 4 = floor4 state
    | otherwise = error "Invalid floor"

hasFloor :: Int -> Bool
hasFloor i = i `elem` [1,2,3,4]

isChip :: Item -> Bool
isChip (Microchip _) = True
isChip _ = False

isGenerator :: Item -> Bool
isGenerator = not . isChip

allChips :: [Item] -> Bool
allChips [] = True
allChips xs = (foldl (&&) True . map isChip) xs

allGenerators :: [Item] -> Bool
allGenerators [] = True
allGenerators xs = (foldl (&&) True . map isGenerator) xs

splitItems :: [Item] -> ([Item], [Item])
splitItems = partition isChip

chipProtected :: [Item] -> Item -> Bool
chipProtected generators (Microchip c) = c `elem` generators'
    where
        generators' = map (\(Generator g) -> g) generators

-- Magic function, that given a list of items, tells if they can be together or not
validCombination :: [Item] -> Bool
validCombination [] = True
validCombination (x:[]) = True
validCombination items
    | allChips items = True
    | allGenerators items = True
    | otherwise =
        (foldl (&&) True . map (chipProtected generators) ) chips
    where
        (chips, generators) = splitItems items

fitInElevator :: [Item] -> Bool
fitInElevator xs
    | length xs > 2 = False
    | null xs = False
    | otherwise = validCombination xs

nextStates :: State -> [State]
nextStates state = [state]
{-
    | null currentFloorCombs = []
    | otherwise =
        (filter validCombination mergedNextFloor) ++
        (filter validCombination mergedPrevFloor)
    where
        currentFloor = getFloor state (elevator state)
        currentFloorCombs :: [[Item]]
        currentFloorCombs = filter fitInElevator (subsequences currentFloor)
        mergedNextFloor :: [[Item]]
        mergedNextFloor = if hasFloor (elevator state + 1)
                          then map ((++) (getFloor state (elevator state + 1))) currentFloorCombs
                          else []
        nextFloorStates = 
        mergedPrevFloor :: [[Item]]
        mergedPrevFloor = if hasFloor (elevator state - 1)
                          then map ((++) (getFloor state (elevator state - 1))) currentFloorCombs
                          else []
                              -}

move :: State -> Int -> Int -> [Item] -> State
move state from to [] = state
move state from to (item:items) = move newState'' from to items
    where
        newState'' = newState' {elevator = to}
        newState' = updateFloor newState from fromFloor'
        newState = updateFloor state to toFloor'

        fromFloor = getFloor state from
        fromFloor' = delete item fromFloor
        toFloor = getFloor state to
        toFloor' = item:toFloor

updateFloor :: State -> Int -> [Item] -> State
updateFloor state i floor
    | i == 1 = state {floor1 = floor}
    | i == 2 = state {floor2 = floor}
    | i == 3 = state {floor3 = floor}
    | i == 4 = state {floor4 = floor}
    | otherwise = error "Invalid floor"

nextStatePossibilities :: [State] -> [[State]]
nextStatePossibilities states = [states]
{-
    | null next = []
    | otherwise = map ((++) states) next
    where
        next = nextStates (last states)
-}

isFinalState :: State -> Bool
isFinalState state
    | (elevator state) /= 4 = False
    | (foldl (&&) True . map (not . null) . map (getFloor state)) [1,2,3] = False
    | otherwise = True

steps :: [State] -> Int
steps prevStates 
    | isFinalState currentState = 0
    | otherwise = 
        1 + ((customMin . map steps . nextStatePossibilities) prevStates)
    where
        currentState = last prevStates
        customMin :: [Int] -> Int
        customMin [] = maxBound
        customMin xs = minimum xs

printState :: State -> IO ()
printState state =
    do
        printFloor state 4
        printFloor state 3
        printFloor state 2
        printFloor state 1
    where
        printFloor :: State -> Int -> IO ()
        printFloor state i =
            do
                putStr $ elevatorStr i state
                print $ getFloor state i

        elevatorStr :: Int -> State -> String
        elevatorStr i state = if (i == elevator state)
                              then "(E) "
                              else "    "

printList :: Show a => (a -> IO ()) -> [a] -> IO ()
printList f [] = return ()
printList f (x:xs) =
    do
        f x
        putStrLn ""
        printList f xs

main = do
    let state = State { elevator = 1
                      , floor4 = []
                      , floor3 = [Generator 'L']
                      , floor2 = [Generator 'H']
                      , floor1 = [Microchip 'H', Microchip 'L']
                      }
        state' = State { elevator = 3
                      , floor4 = [Generator 'H', Microchip 'H']
                      , floor3 = [Generator 'L', Microchip 'L']
                      , floor2 = []
                      , floor1 = []
                      }
        final = State { elevator = 4
                      , floor4 = [Generator 'L', Generator 'H', Microchip 'H', Microchip 'L']
                      , floor3 = []
                      , floor2 = []
                      , floor1 = []
                      }

        problem = State { elevator = 1
                        , floor4 = []
                        , floor3 = [Microchip 'p', Generator 'p', Generator 'R', Microchip 'R']
                        , floor2 = [Microchip 'P', Microchip 'S']
                        , floor1 = [Generator 'T', Microchip 'T', Generator 'P', Generator 'S']
                        }
        -- Promethium => 'p'


    printList printState $ [state, state']
    -- printState state
    -- putStrLn ""
    -- printState $ move state 1 2 [(Microchip 'H'), Microchip 'L']


