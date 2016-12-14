module Main where

import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BS
import qualified Data.Char as C

import Crypto.Hash.MD5

data Hash = Hash Integer BS.ByteString deriving (Show)
data Key = Key Integer deriving (Show)

hashes :: BS.ByteString -> [Hash]
hashes salt =
    map (\(a,b) -> Hash a b) (zip [0,1..] hashes')
    where
        numbers = map (BS.pack . show) [0,1..]
        salted = map (\x -> BS.append salt x) numbers
        hashes' = map (B16.encode . hash) salted

hasTriplet :: Hash -> Bool
hasTriplet (Hash _ str) = hasTriplet' str
    where
        hasTriplet' :: BS.ByteString -> Bool
        hasTriplet' str
            | BS.null str = False
            | BS.isPrefixOf (BS.replicate 3 (BS.head str)) str = True
            | otherwise = hasTriplet' (BS.tail str)

getTripletChar :: Hash -> Char
getTripletChar (Hash _ str) = getTripletChar' str
    where
        getTripletChar' :: BS.ByteString -> Char
        getTripletChar' str
            | BS.isPrefixOf (BS.replicate 3 (BS.head str)) str = BS.head str
            | otherwise = getTripletChar' (BS.tail str)

hasQuintet :: Char -> Hash -> Bool
hasQuintet c (Hash _ str) = hasQuintet' c str
    where
        hasQuintet' :: Char -> BS.ByteString -> Bool
        hasQuintet' c str
            | BS.length str < 5 = False
            | otherwise = BS.isInfixOf (BS.replicate 5 c) str

isKey :: Hash -> [Hash] -> Bool
isKey x xs
    | hasTriplet x = (foldl (||) False . map (hasQuintet (getTripletChar x))) (take 1000 xs)
    | otherwise = False


keys :: [Hash] -> [Key]
keys (x:xs)
    | isKey x xs = (\(Hash i _) -> Key i) x : keys xs
    | otherwise = keys xs

main = do
    let hashes' = hashes (BS.pack "jlmsuwbz")

    print $ keys (hashes') !! 63
