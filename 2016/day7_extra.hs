module Main where

import Data.List

hasAbba :: String -> Bool
hasAbba [] = False
hasAbba (_:[]) = False
hasAbba (_:_:[]) = False
hasAbba (_:_:_:[]) = False
hasAbba (x:s@(y:z:t:xs)) 
    | x == y = hasAbba s
    | (x : y : "") == (t : z : "") = True
    | otherwise = hasAbba s

hypernet :: String -> [String]
hypernet [] = []
hypernet str 
    | ']' `elem` str = firstMatch : hypernet remaining
    | otherwise = []
    where
        firstMatch = (takeWhile (/=']') . tail . dropWhile (/='[')) str
        remaining = (tail . dropWhile (/=']')) str

notHypernet :: String -> [String]
notHypernet [] = []
notHypernet str
    | ']' `elem` str = takeWhile (/='[') str : (notHypernet . tail . dropWhile (/=']')) str
    | otherwise = str : []

supportTLS :: String -> Bool
supportTLS str 
    -- Hass abba in any hypernet
    | (foldl (||) False . map hasAbba . hypernet) str = False
    | otherwise = (foldl (||) False . map hasAbba . notHypernet) str

getAbas :: String -> [String]
getAbas [] = []
getAbas (_:[]) = []
getAbas (_:_:[]) = []
getAbas (x:s@(y:z:xs))
    | x == z && x /= y = (x:y:z:[]) : getAbas s
    | otherwise = getAbas s

allAbas :: String -> [String]
allAbas = foldl (++) [] . map getAbas . notHypernet

abaToBab :: String -> String
abaToBab (x:y:z:[]) = y:x:y:[]

supportSSL :: String -> Bool
supportSSL ip = (foldl (||) False . map (hypernetHasAnyBab babs))  hnets
    where
        hnets = hypernet ip
        babs = (map abaToBab . allAbas) ip

hypernetHasAnyBab :: [String] -> String -> Bool
hypernetHasAnyBab [] hnet = False
hypernetHasAnyBab (x:xs) hnet
    | x `isInfixOf` hnet = True
    | otherwise = hypernetHasAnyBab xs hnet

main = do
    contents <- readFile "input7.txt"
    let contents' = "aba[bab]xyz\nxyx[xyx]xyx\naaa[kek]eke\nzazbz[bzb]cdb\n"
        ips = lines contents
        ssls = filter supportSSL ips

    print $ length ssls
