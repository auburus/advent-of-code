module Main where

import Data.List

data Item' = Generator Char | Chip Char deriving (Show, Eq)
data Item = Item { pos :: Int
                 , item :: Item'
                 } deriving (Show, Eq)

data State = State { moves :: Int
                   , elevator :: Int
                   , items :: [Item]
                   }

instance Show State where
    show (State m elev items) =
        "Moves: " ++ show m ++ "\n" ++
        "Elevator: " ++ show elev ++ "\n" ++
        show (inFloor 4 items) ++ "\n" ++
        show (inFloor 3 items) ++ "\n" ++
        show (inFloor 2 items) ++ "\n" ++
        show (inFloor 1 items) ++ "\n"

sameList :: Eq a => [a] -> [a] -> Bool
sameList [] [] = True
sameList [] _ = False
sameList _ [] = False
sameList (x:xs) ys 
    | x `notElem` ys = False
    | otherwise = sameList xs (delete x ys)

instance Eq State where
    (==) (State m1 e1 items1) (State m2 e2 items2) =
        e1 == e2 && sameList items1 items2

inFloor :: Int -> [Item] -> [Item]
inFloor i = filter (\x -> pos x == i)

isChip :: Item' -> Bool
isChip (Chip _) = True
isChip _ = False

isGenerator :: Item' -> Bool
isGenerator = not . isChip

allChips :: [Item'] -> Bool
allChips [] = True
allChips xs = (foldl (&&) True . map isChip) xs

allGenerators :: [Item'] -> Bool
allGenerators [] = True
allGenerators xs = (foldl (&&) True . map isGenerator) xs

splitItems :: [Item'] -> ([Item'], [Item'])
splitItems = partition isChip

chipProtected :: [Item'] -> Item' -> Bool
chipProtected generators (Chip c) = c `elem` generators'
    where
        generators' = map (\(Generator g) -> g) generators

validFloor :: [Item] -> Bool
validFloor items
    | allChips items' = True
    | allGenerators items' = True
    | otherwise =
        (foldl (&&) True . map (chipProtected generators) ) chips
    where
        items' = map item items
        (chips, generators) = splitItems items'

-- This function checks that an Item collection is a valid one
isValid :: [Item] -> Bool
isValid items = allItemsValid items &&
    (foldl (&&) True
    . map validFloor
    . map (\i -> inFloor i items)) [1..4]
    where
        allItemsValid :: [Item] -> Bool
        allItemsValid = foldl (&&) True
                      . map (\(Item i _) -> i `elem` [1..4] )

-- Until here, validation
-- From now on, the next states
fitInElevator :: [Item] -> Bool
fitInElevator xs
    | length xs > 2 = False
    | null xs = False
    | otherwise = isValid xs

nextStates :: State -> [State]
nextStates state@(State i elev items) =
    map (\(e,x) -> state { moves = i + 1
                         , elevator = e
                         , items = x }) (next elev items)

next :: Int -> [Item] -> [(Int, [Item])]
next current items = total'
    where
        current' = inFloor current items
        validCombs :: [[Item]]
        validCombs = filter fitInElevator (subsequences current')
        withoutComb = map (deleteList items) validCombs
        toUpperFloor = map (map (\x -> x { pos = pos x + 1 } ) ) validCombs
        toLowerFloor = map (map (\x -> x { pos = pos x - 1 } ) ) validCombs
        upperTotal = (zipWith (++) withoutComb toUpperFloor)
        lowerTotal = (zipWith (++) withoutComb toLowerFloor)
        upper = map (\x -> (current + 1, x)) upperTotal
        lower = map (\x -> (current - 1, x)) lowerTotal
        total = upper ++ lower
        total' = filter (isValid . snd) total
        
deleteList :: Eq a => [a] -> [a] -> [a]
deleteList list [] = list
deleteList list (x:xs) = deleteList (delete x list) xs

-- Now, the bsf and the final state check

isFinal :: State -> Bool
isFinal (State m elev items)
    | elev /= 4 = False
    | (null . inFloor 4 ) items = False
    | (not . null . inFloor 1 ) items = False
    | (not . null . inFloor 2 ) items = False
    | (not . null . inFloor 3 ) items = False
    | otherwise = True

bsf :: [State] -> State
bsf queue
    | isFinal current = current
    | otherwise = bsf queue'
    
    where
        current = head queue
        queue' = tail queue ++ (nextStates current)

bsf' :: [State] -> [State] -> State
bsf' prev (x:xs)
    | isFinal x = x
    | otherwise = (bsf' prev' queue')

    where
        next' = nextStates x
        next'' = filter (\x -> x `notElem` prev) next'
        prev' = prev ++ next''
        queue' = xs ++ next''

main = do
    let initial = State 0 1 [ Item 1 (Chip 'H')
                  , Item 1 (Chip 'L')
                  , Item 2 (Generator 'H')
                  , Item 3 (Generator 'L')
                  ]
        initial' = State 0 1 [ Item 1 (Generator 'T')
                             , Item 1 (Chip 'T')
                             , Item 1 (Generator 'P')
                             , Item 1 (Generator 'S')
                             , Item 2 (Chip 'P')
                             , Item 2 (Chip 'S')
                             , Item 3 (Generator 'p')
                             , Item 3 (Chip 'p')
                             , Item 3 (Generator 'R')
                             , Item 3 (Chip 'R')
                             , Item 1 (Chip 'E')
                             , Item 1 (Generator 'E')
                             , Item 1 (Chip 'D')
                             , Item 1 (Generator 'D')
                             ]
        visited = bsf' [initial'] [initial']
        distr i = length . takeWhile (\x -> moves x < i + 1) . dropWhile (\x -> moves x < i)

    print $ bsf' [initial'] [initial']

