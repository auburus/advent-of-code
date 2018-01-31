module Main where

import System.IO (readFile)

import Data.Array (Array)
import qualified Data.Array as A
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (maybe, catMaybes)


type Grid = Array (Integer, Integer) Char
type Cookbook = Map String String

grid2 = A.array ((0,0), (1,1)) [ ((0,0), '.'), ((0,1), '#')
                               , ((1,0), '.'), ((1,1), '.')]
grid3 = A.array ((0,0), (2,2)) [ ((0,0), '.'), ((0,1), '#'), ((0,2), '.')
                               , ((1,0), '.'), ((1,1), '.'), ((1,2), '#')
                               , ((2,0), '#'), ((2,1), '#'), ((2,2), '#')]
grid4 = A.array ((0,0), (3,3)) [ ((0,0), '#'), ((0,1), '.'), ((0,2), '.'), ((0,3), '#')
                               , ((1,0), '.'), ((1,1), '.'), ((1,2), '.'), ((1,3), '.')
                               , ((2,0), '.'), ((2,1), '.'), ((2,2), '.'), ((2,3), '.')
                               , ((3,0), '#'), ((3,1), '.'), ((3,2), '.'), ((3,3), '#')]

cookbook = buildCookbook [ ("...#", "##.#.....")
                         , (".#...####", "#..#........#..#")
                         ]

main = do
    
    contents <- readFile "input21.txt"

    let input = parseInput . lines $ contents
        grid = A.array ((0,0), (2,2)) [ ((0,0), '.'), ((0,1), '#'), ((0,2), '.')
                                      , ((1,0), '.'), ((1,1), '.'), ((1,2), '#')
                                      , ((2,0), '#'), ((2,1), '#'), ((2,2), '#')]
        cookbook = buildCookbook input


    print . doPart1 cookbook $ grid
    print . doPart2 cookbook $ grid

doPart1 :: Cookbook -> Grid -> Int
doPart1 cookbook grid = pixelsOn $ (!!) (iterate (enchance cookbook) grid) 5

doPart2 :: Cookbook -> Grid -> Int
doPart2 cookbook grid = pixelsOn $ (!!) (iterate (enchance cookbook) grid) 18

buildCookbook :: [(String, String)] -> Map String String
buildCookbook [] = M.empty
buildCookbook ((x, y):xs) = 
    foldl (\m (k, v) -> M.insert k v m) (buildCookbook xs)
    . zip (equivalents' x)
    . repeat 
    $ y

parseInput :: [String] -> [(String, String)]
parseInput [] = []
parseInput (x:xs) = 
    let first = takeWhile (/=' ') x
        second = drop 2 . dropWhile (/='>') $ x
        deleteSlash = filter (/= '/')
    in
        (deleteSlash first, deleteSlash second) : parseInput xs

printGrid :: Grid -> IO ()
printGrid grid =
          mapM_ print
          . map (\x -> map snd x)
          . map (\(i, x) -> filter (\((j, _) , _) -> j == i) x)
          . zip [0,1..]
          . replicate (getSize grid)
          . A.assocs
          $ grid

getSize :: Num a => Grid -> a
getSize grid = 1 + (fromInteger . fst . snd . A.bounds $ grid)

equivalents' :: String -> [String]
equivalents' str
    | size == 2 =
        [ str
        , rotate2x2 $ str
        , rotate2x2 . rotate2x2 $ str
        , rotate2x2 . rotate2x2 . rotate2x2 $ str
        , ref2
        , rotate2x2 $ ref2
        , rotate2x2 . rotate2x2 $ ref2
        , rotate2x2 . rotate2x2 . rotate2x2 $ ref2
        ]
    | size == 3 = 
        [ str
        , rotate3x3 $ str
        , rotate3x3 . rotate3x3 $ str
        , rotate3x3 . rotate3x3 . rotate3x3 $ str
        , ref3
        , rotate3x3 $ ref3
        , rotate3x3 . rotate3x3 $ ref3
        , rotate3x3 . rotate3x3 . rotate3x3 $ ref3
        ]

    where
        size = floor . sqrt . fromIntegral . length $ str
        ref2 = reflection2x2 str
        ref3 = reflection3x3 str
        
rotate3x3 :: String -> String
rotate3x3 str =
    [ str !! 6
    , str !! 3
    , str !! 0
    , str !! 7
    , str !! 4
    , str !! 1
    , str !! 8
    , str !! 5
    , str !! 2
    ]

rotate2x2 :: String -> String
rotate2x2 str =
    [ str !! 2
    , str !! 0
    , str !! 3
    , str !! 1
    ]

reflection3x3 :: String -> String
reflection3x3 str =
    [ str !! 2
    , str !! 1
    , str !! 0
    , str !! 5
    , str !! 4
    , str !! 3
    , str !! 8
    , str !! 7
    , str !! 6
    ]

reflection2x2 :: String -> String
reflection2x2 str =
    [ str !! 1
    , str !! 0
    , str !! 3
    , str !! 2
    ]

split :: Grid -> [Grid]
split grid
    | size `mod` 2 == 0 = map (getSubGrid grid 2) . indexesByN 2 $ size
    | size `mod` 3 == 0 = map (getSubGrid grid 3) . indexesByN 3 $ size
    | otherwise = error "error"

    where
        size = getSize grid

merge :: [Grid] -> Grid
merge grids =
    let gridsSize = floor . sqrt . fromIntegral . length $ grids
        gridSize = getSize . head $ grids
        size = gridsSize * gridSize
        v = [0..gridSize-1]
    in
        foldl (\a xs -> a A.// xs) (A.listArray ((0,0), (size-1, size-1)) $ repeat '-')
        . map (\((x,y), g) -> [((x+i, y+j), g A.! (i,j)) | i <- v, j <- v])
        . zip (indexesByN gridSize size)
        $ grids 

indexesByN :: Integer -> Integer -> [(Integer, Integer)]
indexesByN n size = [(i, j) | i <- [0, n..size-n], j <- [0, n..size-n]]

getSubGrid :: Grid -> Integer -> (Integer, Integer) -> Grid
getSubGrid grid size (x,y) = A.array ((0,0), (size-1, size-1))
                           . map (\(i,j) -> ((i-x, j-y), grid A.! (i,j)))
                           . expand size
                           $ (x,y)

expand :: Integer -> (Integer, Integer) -> [(Integer, Integer)]
expand size (x,y) = [(x+a, y+b) | a <- [0..size-1], b <- [0..size-1]]


hashGrid :: Grid -> String
hashGrid = A.elems

unHashGrid :: String -> Grid
unHashGrid str = 
    let size = floor . sqrt . fromIntegral . length $ str
    in 
        A.array ((0,0), (size-1, size-1))
        . zip [(i,j) | i <- [0..size-1], j <- [0..size-1]]
        $ str

transform :: Cookbook -> Grid -> Grid
transform cookbook = unHashGrid
                   . (M.!) cookbook
                   . hashGrid

enchance :: Cookbook -> Grid -> Grid
enchance cookbook = 
    merge
    . map (transform cookbook)
    . split

pixelsOn :: Grid -> Int
pixelsOn = foldl (\s c -> if c == '#' then s+1 else s) 0
         . hashGrid

