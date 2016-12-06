module Main where

import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BS
import qualified Data.Char as C
import qualified Data.Array as Array

import Crypto.Hash.MD5

createInputs :: BS.ByteString -> [BS.ByteString]
createInputs base = map (\x -> BS.append base x) numbers
    where
        -- numbers = map (BS.pack . show) [0,1..]
        numbers = map (BS.pack . show) all
        all :: [Integer]
        all = [0,1..]

relevantPos :: BS.ByteString -> Char
relevantPos = BS.last . BS.take 6

relevantChar :: BS.ByteString -> Char
relevantChar = BS.last . BS.take 7

relevant :: BS.ByteString -> (Int, Char)
relevant bs = (C.digitToInt (relevantPos bs), relevantChar bs)


validHash :: BS.ByteString -> Bool
validHash hash = (fiveZeros hash) && (validPos hash)
    where
        fiveZeros hash = BS.take 5 hash == BS.pack "00000"
        validPos hash = elem (relevantPos hash) ['0'..'7']

printElements :: Show a => [a] -> IO()
printElements [] = return ()
printElements (x:xs) = do print x
                          printElements xs

decrypted :: [(Int, Char)] -> [String]
decrypted pairs = baseStr : zipWith magic (decrypted pairs) pairs
    where
        baseStr = take 8 ['-','-'..]

        magic :: String -> (Int, Char) -> String
        magic passwd (pos, val)
            | passwd !! pos /= '-' = passwd
            | otherwise = firstPart ++ (val : (tail lastPart))
                where (firstPart, lastPart) = splitAt pos passwd
          
main = do
    let inputs = createInputs (BS.pack "ugkcyxxp")
        hashed = map (B16.encode . hash) inputs
        filtered = filter (validHash) hashed 
        relevants = map relevant filtered

    printElements $ decrypted relevants
