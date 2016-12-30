module Main where

import System.IO (readFile)
import Data.Map.Strict (Map)
import qualified Data.Map as Map
import Utils.Queue (Queue)
import qualified Utils.Queue as Q
import Text.Read (readMaybe)
import Data.Bits
import Data.Word (Word16)

data Op = Value String
        | And String String
        | Or String String
        | Not String
        | LShift String Int
        | RShift String Int
        deriving (Eq, Show)

type Ins = (Op, String)
type Registers = Map String Word16

registers :: Registers
registers = Map.empty

parseIns :: String -> Ins
parseIns str = (op, lst)
    where
        splitted = words str
        first = splitted !! 0
        second = splitted !! 1
        third = splitted !! 2
        lst = last splitted
        op  
            | second == "->" = Value first
            | second == "AND" = And first third
            | second == "OR" = Or first third
            | first == "NOT" = Not second
            | second == "LSHIFT" = LShift first (read third)
            | second == "RSHIFT" = RShift first (read third)

execute :: Registers -> Queue Ins -> Registers
execute reg queue
    | Q.null queue = reg
    | otherwise = execute updatedReg updatedIns
    where
        (currentIns, remainingIns) = Q.pop queue
        (updatedReg, updatedIns) =
            case executeOne reg currentIns of
                Nothing -> (reg, Q.push remainingIns currentIns)
                Just registers -> (registers, remainingIns)
            
executeOne :: Registers -> Ins -> Maybe Registers
executeOne reg (op, target) =
    case op of
        Value str1 -> update (getValue reg str1)
        And str1 str2 -> update $ bitOp (.&.) (getValue reg str1) (getValue reg str2)
        Or str1 str2 -> update $ bitOp (.|.) (getValue reg str1) (getValue reg str2)
        Not str1 -> update $ complement' (getValue reg str1) 
        LShift str1 i -> update $ bitOp shift (getValue reg str1) (Just (fromIntegral i))
        RShift str1 i -> update $ bitOp shift (getValue reg str1) (Just (fromIntegral (i*(-1))))
        where
            update val =
                case val of
                    Just i -> Just (Map.insert target i reg)
                    Nothing -> Nothing

complement' :: Maybe Word16 -> Maybe Word16
complement' Nothing = Nothing
complement' (Just i) = Just (complement i)

bitOp :: Integral a => (Word16 -> a -> Word16) -> Maybe Word16 -> Maybe a -> Maybe Word16
bitOp _ Nothing _ = Nothing
bitOp _ _ Nothing = Nothing
bitOp f (Just x) (Just y) = Just (f x (fromIntegral y))

getValue :: Registers -> String -> Maybe Word16
getValue reg str = 
    case readMaybe str :: Maybe Word16 of
        Just i -> Just i
        Nothing -> Map.lookup str reg

problem :: [String] -> Registers
problem = execute registers . Q.fromList . map parseIns

main = do
    contents <- readFile "input7.txt"
    contents2 <- readFile "input7_2.txt"
    let contents' = "123 -> x\n456 -> y\nx AND y -> d\nx OR y -> e\nx LSHIFT 2 -> f\ny RSHIFT 2 -> g\nNOT x -> h\nNOT y -> i"
        input = lines contents
        input' = lines contents2

    print . problem $ input
    putStrLn ""
    print . problem $ input'
