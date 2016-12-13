module Main where

toBinary :: Int -> [Int]
toBinary a
    | a == 0 = [0]
    | a == 1 = [1]
    | otherwise =
        toBinary (a `div` 2) ++ [a `mod` 2]

isOpen :: (Int, Int) -> Bool
isOpen (x,y) =
    let multiplied = x*x + 3*x + 2*x*y + y + y*y
        magicNumber = 10

    in
        (even . sum . toBinary) (multiplied + magicNumber)

isWall :: (Int, Int) -> Bool
isWall = not . isOpen

main = do
    let row = map (\x -> (x,0)) [0..9]
    
    print $ row
    print $ map isOpen row
