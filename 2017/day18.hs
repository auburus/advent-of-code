module Main where

import System.IO (readFile)
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Array (Array)
import qualified Data.Array as A
import Data.Char (isDigit)
import Queue (Queue)
import qualified Queue as Q

data Value = Reg Char | Num Int deriving (Eq, Show) 
data Ins = Snd Value
         | Set Value Value
         | Add Value Value
         | Mul Value Value
         | Mod Value Value
         | Rcv Value
         | Jgz Value Value
         deriving (Eq, Show)

type Registers = Map Char Int
type Instructions = Array Int Ins
type Status = (Registers, Int, Queue Int, Queue Int)

main = do
    contents <- readFile "input18.txt"
    let input = lines contents

    print . doPart1 $ input
    print . doPart2 $ input


doPart1 :: [String] -> Int
doPart1 = findSound M.empty . parseInstructions

doPart2 :: [String] -> Int
doPart2 input =
    let instructions = parseInstructions input
    in countSends instructions ((M.singleton 'p' 0), 0, Q.empty) ((M.singleton 'p' 1), 0, Q.empty)

printList :: Show a => [a] -> IO ()
printList = mapM_ print

parseInstructions :: [String] -> Array Int Ins
parseInstructions xs = A.listArray (0, (length xs) - 1) $ map (toIns . words) xs
    where
        parseVal :: String -> Value
        parseVal x
            | length x >= 2 = Num (read x)
            | isDigit . head $ x = Num (read x)
            | otherwise = Reg (head x)

        toIns :: [String] -> Ins
        toIns ("snd":x:[]) = Snd (parseVal x)
        toIns ("set":x:y:[]) = Set (parseVal x) (parseVal y)
        toIns ("add":x:y:[]) = Add (parseVal x) (parseVal y)
        toIns ("mul":x:y:[]) = Mul (parseVal x) (parseVal y)
        toIns ("mod":x:y:[]) = Mod (parseVal x) (parseVal y)
        toIns ("rcv":x:[]) = Rcv (parseVal x)
        toIns ("jgz":x:y:[]) = Jgz (parseVal x) (parseVal y)

findSound :: Registers -> Instructions -> Int
findSound registers instructions = registers' M.! '_'
    where
        registers' = fst . head . dropWhile (not . emitsSound)
                   $ process registers instructions 0

emitsSound :: (Registers, Ins) -> Bool
emitsSound (regs, (Rcv val))
    | getNum regs val /= 0 = True
    | otherwise = False
emitsSound _ = False

process :: Registers -> Instructions -> Int -> [(Registers, Ins)]
process registers instructions i
    | notElem i . A.range . A.bounds $ instructions = []
    | otherwise = (registers, ins) : process registers' instructions i'
    where
        ins = instructions A.! i
        registers' = updateRegisters registers ins
        i' = i + nextInstruction registers ins

countSends :: Instructions -> (Registers, Int, Queue Int) -> (Registers, Int, Queue Int) -> Int
countSends instructions (registers0, i0, q0) (registers1, i1, q1)
    | isBlocked i0 q0 && isBlocked i1 q1 = 0
    | otherwise = (sends q0' q0'') + countSends instructions (reg0', i0', q0'') (reg1', i1', q1'')

    where
        (reg0', i0', q0', q1') = runUntilWait instructions (registers0, i0, q0, q1)
        (reg1', i1', q1'', q0'') = runUntilWait instructions (registers1, i1, q1', q0')

        isBlocked :: Int -> Queue Int -> Bool
        isBlocked i q
            | notElem i . A.range . A.bounds $ instructions = True
            | otherwise =
                case (instructions A.! i) of
                    Rcv _ -> Q.null q0
                    _ -> False
        
        sends :: Queue Int -> Queue Int -> Int
        sends q updated_q = Q.length updated_q - Q.length q
                

runUntilWait :: Instructions -> Status -> Status
runUntilWait instructions (registers, i, myQueue, otherQueue) =
    case ins of
        Snd a ->
            runUntilWait instructions (registers, i + 1, myQueue, Q.push otherQueue (getNum registers a))
        Rcv (Reg reg) -> if Q.null myQueue
                         then (registers, i, myQueue, otherQueue)
                         else runUntilWait
                                 instructions
                                 ( M.insert reg rcv registers
                                 , i+1
                                 , queue'
                                 , otherQueue)

        _ -> runUntilWait instructions (registers', i', myQueue, otherQueue)

    where
        ins = instructions A.! i
        (rcv, queue') = Q.pop myQueue
        registers' = updateRegisters registers ins
        i' = i + nextInstruction registers ins
        

updateRegisters :: Registers -> Ins -> Registers
updateRegisters registers ins@(Set (Reg reg) _) =
    M.insert reg (newValue registers ins) registers

updateRegisters registers ins@(Add (Reg reg) _) =
    M.insert reg (newValue registers ins) registers

updateRegisters registers ins@(Mul (Reg reg) _) =
    M.insert reg (newValue registers ins) registers

updateRegisters registers ins@(Mod (Reg reg) _) =
    M.insert reg (newValue registers ins) registers

updateRegisters registers ins@(Snd val) =
    M.insert '_' (getNum registers val) registers

updateRegisters registers _ = registers


newValue :: Registers -> Ins -> Int
newValue registers (Set x y) = getNum registers y
newValue registers (Add x y) = (getNum registers x) + (getNum registers y)
newValue registers (Mul x y) = (getNum registers x) * (getNum registers y)
newValue registers (Mod x y) = (getNum registers x) `mod` (getNum registers y)

nextInstruction :: Registers -> Ins -> Int
nextInstruction registers (Jgz val offset)
    | getNum registers val > 0 = getNum registers offset
    | otherwise = 1
nextInstruction _ _ = 1

getNum :: Registers -> Value -> Int
getNum registers (Reg v) = maybe 0 id $ M.lookup v registers
getNum _ (Num n) = n
