module Main where

import System.IO
import Data.Char
import Data.List

-- Room = (Name, Id, Checksum)
type Room = (String, Int, String)

parseRoom :: String -> Room
parseRoom room =
    let (nameWithSlashes, remaining) = span (\x -> x `notElem` (map intToDigit [0..9])) room
        name = filter (\x -> x /= '-') nameWithSlashes
        (id', checksumWithBrackets) = span (\x -> x /= '[') remaining
        id = read id'
        checksum = init $ tail checksumWithBrackets
    in
        (name, id, checksum)

ocurrences :: String -> [(Char, Int)]
ocurrences [] = []
ocurrences s@(x:xs) = (x, length found) : ocurrences others
    where
    (found, others) = partition (== x) s

buildChecksum :: [(Char, Int)] -> String
buildChecksum = 
    let sorted = sortBy customSort

        customSort :: (Char, Int) -> (Char, Int) -> Ordering
        customSort (char1, count1) (char2, count2)
            | count1 == count2 = compare char1 char2
            | otherwise = compare count2 count1
    in 
        map fst . sorted

calcChecksum :: String -> String
calcChecksum = take 5 . buildChecksum . ocurrences

main = do
    contents <- readFile "input4.txt"
    let contents' = "aaaaa-bbb-z-y-x-123[abxyz]\na-b-c-d-e-f-g-h-987[abcde]\ntotally-real-room-200[decoy]"
        rooms = map parseRoom $ lines contents
        correctRooms = filter (\(name, id, checksum) -> calcChecksum name == checksum) rooms
        correctIds = map (\(_, id, _) -> id) correctRooms

    print $ sum correctIds
