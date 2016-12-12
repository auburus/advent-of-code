module Main where

import System.IO

-- I still don't know about monads, and maybe (maybeee) I should for using Map,
-- because the docs are full of references to the Maybe monad
import qualified Data.Map.Strict as Map

data Bin = Output Int | Bot Int deriving (Show)

instance Eq Bin where
    (==) (Output _) (Bot _) = False
    (==) (Output a) (Output b) = a == b
    (==) (Bot a) (Bot b) = a == b

instance Ord Bin where
    compare (Bot a) (Output b) = GT
    compare (Output a) (Bot b) = LT
    compare (Output a) (Output b) = compare a b
    compare (Bot a) (Bot b) = compare a b

createBin :: String -> Int -> Bin
createBin str id
    | str == "output" = Output id
    | str == "bot" = Bot id
    | otherwise = error "Invalid str"

parseRule :: String -> (Bin, (Bin, Bin))
parseRule rule =
    let splitted = words rule
        id :: Int
        id = (read . (!! 1)) splitted
        lowEntity = (!! 5) splitted
        low = (read . (!! 6)) splitted
        highEntity = (!! 10) splitted
        high = (read . (!! 11)) splitted

        bot = createBin "bot" id
        lowBin = createBin lowEntity low
        highBin = createBin highEntity high
    in
        (bot, (lowBin, highBin))
        
parseRules :: [String] -> Map.Map Bin (Bin, Bin)
parseRules inputs =
    let inputs' = filter (\x -> (head . words) x == "bot") inputs
        rules = map parseRule inputs'
        insertRule :: Map.Map Bin (Bin, Bin) -> (Bin, (Bin, Bin)) -> Map.Map Bin (Bin, Bin)
        insertRule map (currentBot, (next1, next2)) =
            Map.insert currentBot (next1, next2) map
    in
        foldl insertRule Map.empty rules

parseValue :: String -> (Bin, Int)
parseValue str =
    let splitted = words str
        val = (read . (!! 1)) splitted
        bot = createBin ((!! 4) splitted) ((read . (!! 5)) splitted)
    in
        (bot, val)

parseValues :: [String] -> Map.Map Bin [Int]
parseValues inputs =
    let inputs' = filter (\x -> (head . words) x == "value") inputs
        iniValues = map parseValue inputs'
    in
        foldl insertVal Map.empty iniValues

minMax :: Ord a => [a] -> (a, a)
minMax xs = (minimum xs, maximum xs)

insertVal :: Map.Map Bin [Int] -> (Bin, Int) -> Map.Map Bin [Int]
insertVal map (bin, val)
    | Map.member bin map =
        Map.adjust (\xs -> xs ++ [val]) bin map
    | otherwise =
        Map.insert bin [val] map

botCanContinue :: Map.Map Bin [Int] -> Bin -> Bool
botCanContinue placements bot = length (placements Map.! bot) == 2

botOperate :: Map.Map Bin (Bin, Bin) -> Map.Map Bin [Int] -> Bin -> Map.Map Bin [Int]
botOperate rules placements bot@(Output _) = placements
botOperate rules placements bot@(Bot _) 
    | (not . botCanContinue placements) bot = placements
    | otherwise =
        insertVal (insertVal placementsWithoutBot (minBot, minVal)) (maxBot, maxVal)
        where
            (minBot, maxBot) = rules Map.! bot
            (minVal, maxVal) = minMax (placements Map.! bot)
            placementsWithoutBot = Map.delete bot placements

steps :: Map.Map Bin (Bin, Bin) -> Map.Map Bin [Int] -> [Map.Map Bin [Int]]
steps rules placements
    | null possibleBots = placements: []
    | otherwise =
        placements : (steps rules placements')
        where
            placements' = botOperate rules placements nextBot
            possibleBots = (Map.keys . Map.filterWithKey fltr) placements
            nextBot = head possibleBots

            fltr (Output _) _ = False
            fltr bot xs = length xs == 2


filterToQuestion :: (Bin, [Int]) -> Bool
filterToQuestion (Output _, _) = False
filterToQuestion (Bot id, xs) =
    61 `elem` xs && 17 `elem` xs

main = do
    contents <- readFile "input10.txt"
    let contents' = "value 5 goes to bot 2\nbot 2 gives low to bot 1 and high to bot 0\nvalue 3 goes to bot 1\nbot 1 gives low to output 1 and high to bot 0\nbot 0 gives low to output 2 and high to output 0\nvalue 2 goes to bot 2"
        inputs = lines contents
        rules = parseRules inputs
        placements = parseValues inputs
        oneList = foldl (++) [] $ map Map.assocs $ steps rules placements

    print $ filter filterToQuestion oneList
