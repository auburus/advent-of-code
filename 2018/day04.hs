module Main where

import System.IO (readFile)
import Data.Text (split, splitOn, pack, unpack)
import Data.List
import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Applicative


data Event = Begin Int
           | Sleeps
           | Wakes
           deriving (Eq, Show)

main :: IO ()
main = do
    input <- toTime . map parseInput . sort . lines <$> readFile "input04.txt"
    let sleeps = map (computeSleeps input . fst)
               . nubBy (\a b -> fst a == fst b)
               $ input

    print . problem1 $ sleeps
    print . problem2 $ sleeps


problem1 :: [(Int, [Int])] -> Maybe Int
problem1 sleeps = 
    let 
        (id, minsleep) = maximumBy (\(_,a) (_,b) -> compare (sum a) (sum b)) $ sleeps
    in
        fmap (*id) $ elemIndex (maximum minsleep) minsleep

problem2 sleeps =
    let guards = map (sleeper sleeps) [0..59]
        index  = 
            elemIndex (maximumBy (\(_, a) (_, b) -> compare a b) guards) $ guards
        sleepiest = fmap (fst . (!!) guards) $ index

    in
        (*) <$> index <*> sleepiest
        

sleeper :: [(Int, [Int])] -> Int -> (Int, Int)
sleeper sleeps min = maximumBy (\(_, a) (_, b) -> compare a b)
                   . map ( \(id, mins) -> (id, mins !! min))
                   $ sleeps
        
    
computeSleeps :: [(Int, [Int])] -> Int -> (Int, [Int])
computeSleeps allnights id =
    (id, foldl (zipWith (+)) [0,0..] . map snd . filter ((==id) . fst) $ allnights)

parseInput :: String -> (Int, Event)
parseInput = mymap . map unpack . split ((flip elem) "[- ]#:") . pack
    where
        mymap [_, _,_,_,h,min,_,_, _, id,_,_] = (0, Begin (read id))
        mymap [_, _,_,_,_,min,_, ev, _] 
            | ev == "wakes" = (read min, Wakes)
            | ev == "falls" = (read min, Sleeps)


toTime :: [(Int, Event)] -> [(Int, [Int])]
toTime [] = []
toTime ((min, (Begin id)):xs) = (id, currentTime min Wakes xs): toTime xs
    where
        currentTime t1 state ((_, Begin _):xs) = take (60-t1) . repeat $ stateToNum state
        currentTime t1 state [] = take (60-t1) . repeat $ stateToNum state
        currentTime t1 state ((t2, state2):xs)
            | state == state2 = currentTime t1 state xs
            | otherwise = (take (t2 - t1) . repeat $ stateToNum state )
                        ++ currentTime t2 state2 xs

        stateToNum Sleeps = 1
        stateToNum Wakes = 0

toTime (_:xs) = toTime xs

