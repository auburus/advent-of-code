module Main where

import Data.Sequence as S

reduce :: S.Seq Int -> Int
reduce seq
    | len == 1 = first
    | otherwise = reduce (dropped |> first)
    where
        len = S.length seq
        toEliminate = len `div` 2
        dropped = S.drop 2 seq
        first = S.index seq 0

reduce' :: S.Seq Int -> Int
reduce' seq
    | len == 1 = first
    | otherwise = reduce' (dropped' |> first)

    where
        len = S.length seq
        toEliminate = len `div` 2
        dropped = dropPos seq toEliminate
        dropped' = S.drop 1 dropped
        first = S.index seq 0

dropPos :: S.Seq a -> Int -> S.Seq a
dropPos seq i = h >< S.drop 1 t
    where
        (h, t) = S.splitAt i seq

main = do
    let input = 3012210
        input' = 5
        seq = S.fromList [1..input]

    print . reduce' $ seq
