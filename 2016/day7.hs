module Main where

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

main = do
    contents <- readFile "input7.txt"
    let contents' = "abba[mnop]qrst\nabcd[bddb]xyyx\naaaa[qwer]tyui\nioxxoj[asdfgh]zxcvbn\n"
        ips = lines contents
        ipsTLS = filter supportTLS ips

    print $ length ipsTLS
