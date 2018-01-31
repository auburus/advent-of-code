module Main where

import System.IO (readFile)
import Data.List
import Data.Ord
import Data.List.Split (splitOn)

type Vec = (Integer, Integer, Integer)
type Pos = Vec
type Speed = Vec
type Acc = Vec
type Particle = (Int, (Pos, Speed, Acc))
type Time = Int
type Colision = (Vec, Time)

main = do
    contents <- readFile "input20.txt"

    let input = zip [0,1..] . map parseInput . lines $ contents

    print . take 10 . closestLongTerm $ input
    print . length . simulateWithCollisions 10000 $ input

parseInput :: String -> (Pos, Speed, Acc)
parseInput s = (takeCoords pos, takeCoords spd, takeCoords acc)
    where
        [pos, spd, acc] = splitOn " " s

        takeCoords :: String -> (Integer, Integer, Integer)
        takeCoords xs = 
            let initial = takeWhile (/='>') . tail . dropWhile (/='<') $ xs
                [x, y, z] = map read . splitOn "," $ initial
            in (x, y, z)

closestLongTerm :: [Particle] -> [Particle]
closestLongTerm = minimumSpd . minimumAcc

minimumAcc :: [Particle] -> [Particle]
minimumAcc xs = 
    let sorted = sortBy (\(_, a) (_, b) -> compare a b)
               . map (\p@(i,x) -> (i, (accModule p)))
               $ xs
        minAcc = map fst . filter (\(_, a) -> a == (snd . head $ sorted)) $ sorted
    in
        filter (\(i, _) -> i `elem` minAcc) xs

minimumSpd :: [Particle] -> [Particle]
minimumSpd xs =
    let sorted = sortBy (\(_, a) (_, b) -> compare a b)
               . map (\p@(i, x) -> (i, (spdModule p)))
               $ xs
        minSpd = map fst . filter (\(_, spd) -> spd == (snd . head $ sorted)) $ sorted
    in
        filter(\(i, _) -> i `elem` minSpd) xs

simulateWithCollisions :: Int -> [Particle] -> [Particle]
simulateWithCollisions 0 xs = xs
simulateWithCollisions i xs = simulateWithCollisions (i-1)
                            . removeCollisions
                            . nextTick 
                            $ xs

nextTick :: [Particle] -> [Particle]
nextTick = map update

update :: Particle -> Particle
update (i, ((px, py, pz), (vx, vy, vz), (ax, ay, az))) =
    (i, ((px + vx', py + vy', pz + vz'), (vx', vy', vz'), (ax, ay, az)))
    where
        (vx', vy', vz') = (vx + ax, vy + ay, vz + az)

removeCollisions :: [Particle] -> [Particle]
removeCollisions [] = []
removeCollisions (x@(_, (x_pos, _, _)):xs)
    | null collided = x : removeCollisions xs
    | otherwise = removeCollisions remaining
    where
        (collided, remaining) = partition (\(_, (pos, _, _)) -> pos == x_pos) xs


distance :: Particle -> Integer
distance (_, ((x, y, z), _, _)) = x + y + z

accModule :: Particle -> Integer
accModule (_, (_, _, acc)) = getModule acc

spdModule :: Particle -> Integer
spdModule (_, (_, spd, _)) = getModule spd

getModule :: Vec -> Integer
getModule (x, y, z) = abs x + abs y + abs z


colisionTime :: Integral a => a -> a -> a -> Maybe Time
colisionTime a b c
    | b'*b' < 4 * a' * c' = Nothing
    | b'*b' == 4 * a' * c' = 
        case isIntegral (-b' / (2 * a')) of
            True -> Just (round (-b' / (2*a')))
            False -> Nothing
    | otherwise = Nothing

    where
        a' = fromIntegral a
        b' = fromIntegral b
        c' = fromIntegral c
        sol1 = (-b' + sqrt (b'*b' - 4*a'*c')) / (2 * a')
        sol2 = (-b' - sqrt (b'*b' - 4*a'*c')) / (2 * a')

isIntegral :: RealFrac a => a -> Bool
isIntegral x = floor x == ceiling x
