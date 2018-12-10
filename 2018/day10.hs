{-# LANGUAGE BangPatterns #-}

module Main where

import Data.List
import Data.List.Split (splitOneOf)
import System.IO (readFile)
import Data.Text (strip)
import Data.Set (Set)
import qualified Data.Set as S

type Pos = (Int, Int)
type Speed = (Int, Int)
type Pos' = (Double, Double)

main = do
    input <- map parseInput . lines <$> readFile "input10.txt" 
    let (message, i) = run input

    printCoords $ message
    print $ i

parseInput :: String -> (Pos, Speed)
parseInput = parse . map (dropWhile (==' ')) . splitOneOf "<,>"
    where
        parse [_,x,y,_,vx,vy,_] = ((read x, read y), (read vx, read vy))

run :: [(Pos, Speed)] -> ([(Pos, Speed)], Int)
run xs = doRun 0 xs (tic xs)
    where
        doRun !i !x !y
            | varPos x < varPos y = (x, i)
            | otherwise = doRun (i+1) y (tic y)

tic :: [(Pos, Speed)] -> [(Pos, Speed)]
tic = map (\((x,y),(vx,vy)) -> ((x+vx, y+vy), (vx, vy)))


meanPos :: [(Pos, Speed)] -> Pos'
meanPos xs = ( mean . map (fst . fst) $ xs, mean . map (snd . fst) $ xs )

varPos :: [(Pos, Speed)] -> Pos'
varPos xs = (var . map (fst . fst) $ xs, var . map (snd . fst) $ xs)

mean :: Integral a => [a] -> Double
mean xs = (fromIntegral . sum $ xs) / (fromIntegral . length $ xs)

var :: Integral a => [a] -> Double
var xs =
    let m = mean xs
    in (sum . map ((^2) . ((-)m) . fromIntegral) $ xs) / (fromIntegral . length $ xs)

printCoords :: [(Pos, Speed)] -> IO ()
printCoords p = 
    let xs = map (fst . fst) p
        ys = map (snd . fst) p
        (minX, maxX) = (minimum xs, maximum xs)
        (minY, maxY) = (minimum ys, maximum ys)
        coords = foldl (flip S.insert) S.empty . map fst $ p
        line j = [ if (i,j) `S.member` coords then '#' else ' ' | i <- [minX..maxX]]
    in
        do
            mapM_ putStrLn . map line $ [minY..maxY]

