module Main where

import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Queue (Queue)
import qualified Queue as Q

data Elem = Hidrogen
          | Lithium
          | Thulium
          | Plutonium
          | Strontium
          | Promethium
          | Ruthenium
          deriving (Show, Eq, Ord)

data Item = Gen Elem
          | Chip Elem
          | Pair
          deriving (Show, Eq, Ord)

type Floor = Int
type Elev = Int

type Items = [(Floor, Item)]
type Area = (Elev, Items)

type Moves = Int

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

-- Toca optimitzar aquesta funcio...
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

isFinalArea :: Area -> Bool
isFinalArea (elev, items)
    | elev /= 4 = False
    | foldl (&&) True . map (\i -> null . getFloor i $ items) $ [1,2,3] = True
    | otherwise = False

-- TODO
bfs :: Set Area -> Queue (Moves, Area) -> Moves
bfs visited queue
    | isFinalArea area = moves
    | otherwise = bfs visited' queue''
    where
        ((moves, area), queue') = Q.pop queue
        next = nextAreas area
        next' = filter (\area -> not . Set.member area $ visited) next
        visited' = foldl (\set area -> Set.insert area set) visited next'
        queue'' = foldl Q.push queue' . map (\x -> (moves + 1, x)) $ next'

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

problem1 :: Int
problem1 = bfs (Set.insert initial Set.empty) (Q.queue [(0, initial)])
    where
        initial = ( 1, [ (1, Gen Thulium)
                       , (1, Chip Thulium)
                       , (1, Gen Plutonium)
                       , (1, Chip Plutonium)
                       , (1, Gen Strontium)
                       , (2, Chip Plutonium)
                       , (2, Chip Strontium)
                       , (3, Gen Promethium)
                       , (3, Chip Promethium)
                       -- , (3, Gen Ruthenium)
                       -- , (3, Chip Ruthenium)
                       ])
main = do
    print problem1
