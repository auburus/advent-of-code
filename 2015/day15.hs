module Main where

import Data.List
import Data.Array (Array, Ix)
import qualified Data.Array as A

data Ingredient = Sugar
                | Sprinkles
                | Candy
                | Chocolate
                deriving (Eq, Ord, Show, Ix)

data Props = Props { cap :: Int
                   , dur :: Int
                   , flav :: Int
                   , text :: Int
                   , cal :: Int
                   } deriving (Eq, Show)

instance Num Props where
    (+) prop1 prop2 = Props { cap = cap prop1 + cap prop2
                            , dur = dur prop1 + dur prop2
                            , flav = flav prop1 + flav prop2
                            , text = text prop1 + text prop2
                            , cal = cal prop1 + cal prop2 }

type Data = Array Ingredient Props

incr :: [Int] -> [Int]
incr [] = []
incr (x:xs)
    | x == 100 = 0 : incr xs
    | otherwise = (x+1):xs


combs :: [[(Ingredient, Int)]]
combs = [[(Sugar,x), (Sprinkles, y), (Candy, z), (Chocolate, t)] |
        x <- [0..99], y <- [0..99], z <- [0..99], t <- [0..99], x+y+z+t == 100]


propMultiply :: Props -> Int -> Props
propMultiply props i = Props { cap = cap props * i
                             , dur = dur props * i
                             , flav = flav props * i
                             , text = text props * i
                             , cal = cal props * i }

batchProps :: Data -> [(Ingredient, Int)] -> Props
batchProps dat [] = Props {cap= 0, dur= 0, flav= 0, text= 0, cal= 0}
batchProps dat ((ingr, qty):xs) = (propMultiply (dat A.! ingr) qty) + batchProps dat xs

fixBatch :: Props -> Props
fixBatch props = Props { cap = max 0 (cap props)
                       , dur = max 0 (dur props)
                       , flav = max 0 (flav props)
                       , text = max 0 (text props)
                       , cal = cal props }

score :: Props -> Int
score prop = cap prop * dur prop * flav prop * text prop

problem1 :: Data -> Int
problem1 dat = maximum .
               map (score . fixBatch . batchProps dat) $ combs

problem2 :: Data -> Int
problem2 dat = maximum .
               map (score . fixBatch) .
               filter (\x -> cal x == 500) .
               map (batchProps dat) $ combs

main = do
    let contents = [ (Sugar,     Props {cap= 3 , dur= 0, flav=  0, text= -3, cal= 2})
                   , (Sprinkles, Props {cap= -3, dur= 3, flav=  0, text=  0, cal= 9})
                   , (Candy,     Props {cap= -1, dur= 0, flav=  4, text=  0, cal= 1})
                   , (Chocolate, Props {cap= 0 , dur= 0, flav= -2, text=  2, cal= 8})
                   ]

        input :: Data
        input = A.array (Sugar, Chocolate) contents
                
    print . problem1 $ input
    print . problem2 $ input
