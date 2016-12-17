module Main where

import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BS
import Crypto.Hash.MD5

type Steps = BS.ByteString
data Pos = Pos Int Int deriving (Show, Eq)

openDoors :: BS.ByteString -> Steps -> [Bool]
openDoors password = map (\c -> c `elem` ['b','c','d','e','f']) .
            BS.unpack .
            BS.take 4 .
            B16.encode .
            hash .
            BS.append password

nextPos :: BS.ByteString -> (Steps, Pos) -> [(Steps, Pos)]
nextPos password (steps, Pos x y) = filter notWall next'
    where
        notWall = \(_,(Pos x y)) -> x `elem` [0..3] && y `elem` [0..3]
        doorIsOpen = \(steps, _) -> True
        next = (BS.snoc steps 'U', Pos x (y-1)) :
               (BS.snoc steps 'D', Pos x (y+1)) :
               (BS.snoc steps 'L', Pos (x-1) y) :
               (BS.snoc steps 'R', Pos (x+1) y) : []
        next' = snd .
                unzip .
                filter (\(b, _) -> b) .
                zip (openDoors password steps) $ next


bsf :: BS.ByteString -> [(Steps, Pos)] -> [Int]
bsf _ [] = []
bsf password queue@(x:xs) 
    | snd x == (Pos 3 3) = BS.length (fst x) : bsf password xs
    | otherwise = bsf password (next queue)
    where
        next :: [(Steps, Pos)] -> [(Steps, Pos)]
        next (x:xs) = xs ++ (nextPos password x)

main = do
    let password = BS.pack "pxxbnzuo"

    print . maximum . bsf password $ [(BS.empty , Pos 0 0)]
