module Main where

-- This only works in ghci or with increased
-- stack space
--
-- Compile it with -rtsopts
-- Run it with ./a.out +RTS -K128M

elves :: [Int]
elves = 1: zipWith get [2,3..] elves

get :: Int -> Int -> Int
get n an'
    | an' == (n-1) = 1 
    | an' <= (half n) - 2 = an' + 1
    | otherwise = an' + 2
    where
        half n = n `div` 2 + 1

main = do
    let input = 3012210

    print $ elves !! (input-1)
