module Main where

import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Queue (Queue)
import qualified Queue as Q
import Data.Array (Array)
import qualified Data.Array as A

data Elem = Hidrogen
          | Lithium
          | Thulium
          | Plutonium
          | Strontium
          | Promethium
          | Ruthenium
          | Elerium
          | Dilithium
          deriving (Show, Eq, Ord)

data Item = Gen Elem
          | Chip Elem
          | Pair
          deriving (Show, Eq, Ord)

type Floor = Int
type Elev = Int

type House = Array Int [Item]
type Area = (Elev, House)

type Moves = Int

validElevator :: Int -> Bool
validElevator i = i >= 1 && i<= 4

getFloor :: Int -> House -> [Item]
getFloor i house = house A.! i

getCurrentFloor :: Area -> [Item]
getCurrentFloor area = getFloor (fst area) (snd area)

inElevator :: [Item] -> Bool
inElevator items
    | length items == 0 || length items > 2 = False
    | otherwise = validFloor items

floorCombs :: [Item] -> [([Item], [Item])]
floorCombs items = filter (\(a, b) -> inElevator a && validFloor b) .
                   map (makePairs items) .
                   subsequences $ items
    where
        makePairs :: [Item] -> [Item] -> ([Item], [Item])
        makePairs all sel = (sel, remove all sel)
            where
                remove all [] = all
                remove all (x:xs) = remove (delete x all) xs

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

validFloor :: [Item] -> Bool
validFloor items = allChipsProtected chips generators
    where
        (chips, generators) = partition isChip items

validItems :: House -> Bool
validItems items = foldl (&&) True .
                   map (\f -> (validFloor . f) items) $
                   map (\i -> getFloor i) [1..4]

validArea :: Area -> Bool
validArea = validItems . snd

nextAreas :: Area -> [Area]
nextAreas area@(elev, house)
    | elev == 1 = areasUp
    | elev == 4 = areasDown
    | otherwise = areasUp ++ areasDown
    where
        current = getCurrentFloor area
        combs :: [([Item], [Item])]
        combs = floorCombs current
        combsUp = map (\(toMove, rem) -> house A.// [(elev, rem), (elev + 1, sort $ addMultiple (getFloor (elev + 1) house) toMove)]) combs
        combsDown = map (\(toMove, rem) -> house A.// [(elev, rem), (elev - 1, sort $ addMultiple (getFloor (elev - 1) house) toMove)]) combs

        areasUp = filter validArea $ map (\x -> (elev + 1, x)) combsUp
        areasDown = filter validArea $ map (\x -> (elev - 1, x)) combsDown

        addMultiple all [] = all
        addMultiple all (x:xs) = addMultiple (x : all) xs
        
        
isFinalArea :: Area -> Bool
isFinalArea (elev, house)
    | elev /= 4 = False
    | foldl (&&) True . map (\i -> null . getFloor i $ house) $ [1,2,3] = True
    | otherwise = False

-- Functions to convert pairs of elements to an element called Pair
shrinkArea :: Area -> Area
shrinkArea (elev, house) = -- (elev, house) {-
    (elev, (A.//) house $ map (\i -> (i, combine . splitFloor . getFloor i $ house)) [1..4])
    where
        combine :: ([Item] ,[Item]) -> [Item]
        combine (chips, []) = chips
        combine ([], gens) = gens
        combine (((Chip c):chips), gens) =
            case find (\(Gen g) -> g == c) gens of
                Nothing -> (Chip c) : combine (chips, gens)
                Just gen -> Pair : combine (chips, (delete gen gens))

        splitFloor = partition isChip
        -- -}

bfs :: Set Area -> Queue (Moves, Area) -> [(Moves, Area)]
bfs visited queue
    | isFinalArea area = (moves, area) : []
    | otherwise = -- (moves, area) :
        bfs visited' queue''
    where
        ((moves, area), queue') = Q.pop queue
        next = nextAreas area
        shrinked = map shrinkArea next
        next' = filter (\(_, shrink) -> Set.notMember shrink visited) . zip next $ shrinked
        visited' = foldl (\set (_, shrink) -> Set.insert shrink set) visited next'
        queue'' = foldl Q.push queue' . map (\x -> (moves + 1, x)) $ map fst next'

printList :: Show a => [a] -> IO ()
printList [] = return ()
printList (x:xs) = do
    print x
    printList xs

initial :: Area
initial = ( 1, A.array (1,4) [ (1, sort [Chip Lithium, Chip Hidrogen])
                             , (2, sort [Gen Hidrogen])
                             , (3, sort [Gen Lithium])
                             , (4, [])
                             ])

--problem1 :: Int
problem1 = bfs (Set.insert (shrinkArea initial) Set.empty) (Q.queue [(0, initial)])
    where
        initial = ( 1, A.array (1,4) [ (1, sort [Gen Thulium, Chip Thulium, Gen Plutonium, Gen Strontium])
                                     , (2, sort [Chip Plutonium, Chip Strontium])
                                     , (3, sort [Gen Promethium, Chip Promethium, Gen Ruthenium, Chip Ruthenium])
                                     , (4, [])
                                     ])
--problem2 :: Int
problem2 = bfs (Set.insert initial Set.empty) (Q.queue [(0, initial)])
    where
        initial = ( 1, A.array (1,4) [ (1, sort [ Gen Thulium, Chip Thulium, Gen Plutonium, Gen Strontium
                                           , Gen Elerium, Chip Elerium, Gen Dilithium, Chip Dilithium])
                                     , (2, sort [Chip Plutonium, Chip Strontium])
                                     , (3, sort [Gen Promethium, Chip Promethium, Gen Ruthenium, Chip Ruthenium])
                                     , (4, [])
                                     ])

main = do
    printList $ bfs (Set.insert initial Set.empty) (Q.queue [(0, initial)])
    -- printList $ problem1
    print $ problem1
    print $ problem2
