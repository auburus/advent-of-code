module Main where

{-
t+1 = 13*n + 2 <=> t = 11*n + 1
t+2 = 5*n + 0
t+3 = 17*n + 6
t+4 = 3*n + 0
t+5 = 7*n + 5
t+6 = 19*n + 2
-}

passWheel :: (Int, Int) -> Int -> Bool
passWheel (offset, period) x = (x - offset) `mod` period == 0

wheels :: [(Int, Int)]
wheels = [ (13-11-1, 13)
         , (5-0-2, 5)
         , (17-11-3, 17)
         , (3-0-4, 3)
         , (7-2-5, 7)
         , (19-17-6, 19)
         ]

wheels' :: [(Int, Int)]
wheels' = [ (0, 5)
          , (-1, 2)
          ]

main = do
    let numbers = [0,1..]
        numbers' = replicate 6 numbers
        combos = zipWith (\wheel numb -> map (passWheel wheel) numb) wheels numbers'
        firstCommon = foldl (zipWith (&&)) (map (\_ -> True) [0,1..]) combos
        zipped = zip numbers firstCommon


    print $ head (filter (\(_,b) -> b) zipped)

