module Main where

import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BS
import qualified Data.Char as C

import Crypto.Hash.MD5

createInputs :: BS.ByteString -> [BS.ByteString]
createInputs base = map (\x -> BS.append base x) numbers
    where
        numbers = map (BS.pack . show) [0,1..]

isRelevant :: BS.ByteString -> Bool
isRelevant bs = BS.take 5 hashed == BS.pack "00000"
    where
        hashed = (B16.encode . hash) bs

main = do
    let inputs = createInputs (BS.pack "bgvyzdsv")
        filtered = filter isRelevant inputs 

    print $ take 1 filtered
