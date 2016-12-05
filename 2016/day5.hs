module Main where

import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BS
import qualified Data.Char as C

import Crypto.Hash.MD5

createInputs :: BS.ByteString -> [BS.ByteString]
createInputs base = map (\x -> BS.append base x) numbers
    where
        numbers = map (BS.pack . show) [0,1..]

relevantChar :: BS.ByteString -> Char
relevantChar = BS.last . BS.take 6

main = do
    let inputs = createInputs (BS.pack "ugkcyxxp")
        hashed = map (B16.encode . hash) inputs
        filtered = filter (\x -> (BS.take 5 x) == (BS.pack "00000")) hashed 
        relevant = map relevantChar filtered

    print $ take 8 relevant
