module Main where

import System.IO (readFile)
import Data.Array (Array)
import qualified Data.Array as A
import Control.Applicative
import Data.List
import Data.Set (Set)
import qualified Data.Set as S
import Data.Tuple (swap)

type Acted = Bool
type Pos = (Int, Int)
data Faction = Elf | Goblin deriving (Eq, Show)

data Unit = Unit { position :: Pos
                 , health :: Int
                 , faction :: Faction
                 , acted :: Bool
                 , strength :: Int
                 } deriving (Show, Eq)

instance Ord Unit where
    a `compare` b = (swap $ position a) `compare` (swap $ position b)

main = do
    input <- toArray <$> readFile "input15.txt"
    let units = getUnits input
        grid = clearMap input units
        units1 = turn grid units

    -- mapM_ print . S.toList . (flip (!!)) 23 $ iterate (turn grid) units
    print $ problem1 grid units
    print $ problem2 grid units
    -- mapM_ print . S.toList $ (turn grid) units1

    where
        toArray :: String -> Array (Int, Int) Char
        toArray s = 
            let n = flip (-) 1 . length . lines $ s
                m = flip (-) 1 . length . head . lines $ s
            in
            A.array ((0,0), (n, m))
            . zip [(j, i) | i <- [0..n], j <- [0..m]]
            . foldl1 (++) . lines $ s


problem1 grid units =
    (\(n, units) -> (n* (foldl (+) 0 . map health $ S.toList units)))
    $ battle grid units 0

problem2 grid units
    | S.size elves == surviving_elves =
        n* (foldl (+) 0 . map health $ S.toList units')
    | otherwise =
        problem2 grid (S.union elves' goblins)

    where
        (n, units') = battle grid units 0
        surviving_elves = S.size . S.filter ((==Elf) . faction) $ units'
        (elves, goblins) = S.partition ((==Elf) . faction) units
        elves' = S.map (\e -> e {strength= (strength e + 1)} ) elves


battle grid units n
    | S.null $ S.filter ((==Elf) . faction) units' = (n, units')
    | S.null $ S.filter ((==Goblin) . faction) units' = (n, units')
    | otherwise =
        battle grid units' (n+1)

    where
        units' = (turn grid units)

getUnits :: Array (Int, Int) Char -> Set Unit
getUnits input =
    let toFaction 'G' = Goblin
        toFaction 'E' = Elf
    in
        S.fromList
        . map (\(p, c) ->
            Unit { position=p, health=200, faction=toFaction c, acted = False, strength=3})
        . filter ((flip elem "GE") . snd) . A.assocs $ input 
        
clearMap :: Array (Int, Int) Char -> Set Unit -> Array (Int, Int) Char
clearMap input units =
    (A.//) input . zip (map position $ S.toList units) $ repeat '.'


turn :: Array (Int, Int) Char -> Set Unit -> Set Unit
turn grid units
    | S.null $ S.filter (not . acted) units = S.map (\u -> u {acted=False} ) units
    | otherwise = turn grid $ S.insert (current' {acted=True}) attacked
    -- | otherwise = S.insert (current' {acted = True} ) attacked
    where
        current = S.findMin $ S.filter (not . acted) units
        others = S.delete current units
        current' = move grid current others
        attacked = attack grid current' others


-- move :: Array (Int, Int) Char -> 
move grid unit others =
    let enemies = S.filter ((/=(faction unit)) . faction) $ others
        grid' = (A.//) grid . (flip zip) (repeat 'u') . map position $ S.toList others
        targets = nub
                . filter ((=='.') . (A.!) grid')
                . foldl (++) []
                . map neighbours
                . S.toList
                . S.map position
                $ enemies

    in
        case bsf grid' targets (position unit) of
            Nothing -> unit
            Just pos -> unit {position = pos}

bsf :: Array (Int, Int) Char -> [Pos] -> Pos -> Maybe Pos
bsf _ [] _ = Nothing
bsf grid targets pos
    | pos `elem` targets = Nothing
    | otherwise = bsf' (S.fromList $ initial) $ map (\a -> (a,a)) initial
    
    where
        initial = filter ((=='.') . (A.!) grid)
                $ neighbours pos
        
        bsf' :: Set Pos -> [(Pos, Pos)] -> Maybe Pos
        bsf' _ [] = Nothing
        bsf' seen ((m, x):xs)
            | x `elem` targets = Just m
            | otherwise = bsf' (S.union seen $ S.fromList new')
                        . (++) xs
                        $ map (\a -> (m,a)) new'
            where
                new' = filter (flip S.notMember seen)
                     . filter ((=='.') . (A.!) grid)
                     $ neighbours x


attack :: Array (Int, Int) Char -> Unit -> Set Unit -> Set Unit
attack grid unit others
    | S.null enemies = others
    | health target' <= 0 = S.delete target others
    | otherwise = S.insert target' others

    where
        enemies = S.filter (flip elem (neighbours $ position unit) . position)
                . S.filter ((/=(faction unit)) . faction)
                $ others
        target = minimumBy (\a b -> (health a) `compare` (health b)) $ S.toList enemies
        target' = target { health = (health target - strength unit) }

neighbours :: Pos -> [Pos]
neighbours (x,y) = [(x, y-1), (x-1, y), (x+1, y), (x, y+1)]


