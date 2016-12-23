module Main where

import Data.List

data Elem = Hidrogen
          | Lithium
          deriving (Show, Eq, Ord)

data Item = Gen Elem
          | Chip Elem
          deriving (Show, Eq, Ord)

type Floor = Int
type Elev = Int

type Items = [(Floor, Item)]
type Area = (Elev, Items)

validElevator :: Int -> Bool
validElevator i = i >= 1 && i<= 4

getFloor :: Int -> Items -> Items
getFloor i = filter ((==i) . fst)

getCurrentFloor :: Area -> Items
getCurrentFloor area = getFloor (fst area) (snd area)

partFloor :: Int -> Items -> (Items, Items)
partFloor i = partition ((==i) . fst)

partCurrentFloor :: Area -> (Items, Items)
partCurrentFloor area = partFloor (fst area) (snd area)

inElevator :: Items -> Bool
inElevator items
    | length items == 0 || length items > 2 = False
    | otherwise = validFloor items

floorCombs :: Items -> [(Items, Items)]
floorCombs items = filter (inElevator . fst) .
                   map (makePairs items) .
                   subsequences $ items
    where
        makePairs :: Items -> Items -> (Items, Items)
        makePairs all sel = (sel, remove all sel)
            where
                remove all [] = all
                remove all (x:xs) = remove (delete x all) xs

moveUp :: Items -> Items
moveUp = map (\(floor, item) -> (floor + 1, item))

moveDown :: Items -> Items
moveDown = map (\(floor, item) -> (floor - 1, item))

-- Validation related stuff
isChip :: Item -> Bool
isChip (Chip _) = True
isChip _ = False

allChipsProtected :: [Item] -> [Item] -> Bool
allChipsProtected _ [] = True
allChipsProtected [] _ = True
allChipsProtected (Chip c:chips) gen 
    | Gen c `elem` gen = allChipsProtected chips gen
    | otherwise = False

validFloor :: Items -> Bool
validFloor items = allChipsProtected chips generators
    where
        items' = map snd items
        (chips, generators) = partition isChip items'

validItems :: Items -> Bool
validItems items = foldl (&&) True .
                   map validFloor .
                   map (\f -> f items) $
                   map (\i -> getFloor i) [1..4]

validArea :: Area -> Bool
validArea = validItems . snd

-- A partir d'aqui, es nomes anar calculant les seguents arees i filtrar a saco.
-- Probblement, mirar com van les arees en un set i aquestes cosa...
nextAreas :: Area -> [Area]
nextAreas area
    | fst area == 1 = areasUp
    | fst area == 4 = areasDown
    | otherwise = areasUp ++ areasDown
    where
        (current, rest) = partCurrentFloor area
        (partToMove, partToStay) = unzip . floorCombs $ current
        combsUp = zipWith (++) (map moveUp partToMove) partToStay
        combsDown = zipWith (++) (map moveDown partToMove) partToStay
        combsUp' = map (\x -> sort (x ++ rest)) combsUp
        combsDown' = map (\x -> sort (x ++ rest)) combsDown

        areasUp = filter validArea $ map (\x -> ((fst area + 1), x)) combsUp'
        areasDown = filter validArea $ map (\x -> ((fst area - 1), x)) combsDown'

-- TODO
-- bfs

printList :: Show a => [a] -> IO ()
printList [] = return ()
printList (x:xs) = do
    print x
    printList xs

initial :: Area
initial = ( 1, [ (1, Chip Lithium)
               , (1, Chip Hidrogen)
               , (2, Gen Hidrogen)
               , (3, Gen Lithium)
               ])

main = do
    printList . nextAreas $ initial
