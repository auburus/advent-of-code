module Main where

import Data.Array (Array)
import qualified Data.Array as Array
import Data.List
import Data.List.Split (splitOn, wordsBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (Maybe)
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import System.IO

type Range = (Int, Int)

type Field = (String, Range, Range)

type Ticket = [Int]

readInput :: String -> IO ([Field], Ticket, [Ticket])
readInput f = do
  (section1 : section2 : section3 : []) <- fmap (splitOn "\n\n") . readFile $ f
  let fields = map parseField . lines $ section1
      ticket = map read . splitOn "," . last . lines $ section2 :: Ticket
      tickets = map (map read . splitOn ",") . tail . lines $ section3 :: [Ticket]

  return (fields, ticket, tickets)

parseField :: String -> Field
parseField str =
  let (name, str') = span (/= ':') str
      ranges = map parseRange . splitOn " or " $ drop 2 str'
   in (name, head ranges, last ranges)

parseRange :: String -> Range
parseRange str =
  let (r1 : r2 : []) = map read . splitOn "-" $ str
   in (r1, r2)

main = do
  input@(fields, ticket, tickets) <- readInput "input16.txt"

  print $ problem1 input

  print $ problem2 input

problem1 (fields, ticket, tickets) = foldl (+) 0 . foldl (++) [] $ map (invalidValues fields) tickets

problem2 (fields, ticket, tickets) =
  let tickets' = filter (isValid fields) tickets
      columns = [map (!! i) (ticket : tickets') | i <- [0 .. (length fields - 1)]]
      assignedFields = map (fst' . head) . head . dropWhile (any ((> 1) . length)) . iterate reduceAssignedFields $ map (flip validFields fields) columns
   in foldl (*) 1 . map snd . filter (\(a, _) -> "departure" `isInfixOf` a) $ zip assignedFields ticket

reduceAssignedFields :: [[Field]] -> [[Field]]
reduceAssignedFields xs =
  let assignedFields = foldl (++) [] . filter ((== 1) . length) $ xs
   in map (\a -> if length a == 1 then a else filter (flip notElem assignedFields) a) xs

validFields :: [Int] -> [Field] -> [Field]
validFields values fields = filter (fieldFitData values) fields

isValid :: [Field] -> Ticket -> Bool
isValid fields ticket = all (canBeValid fields) $ ticket

invalidValues :: [Field] -> Ticket -> [Int]
invalidValues fields values = filter (not . canBeValid fields) values

-- hasInvalidValues :: Ticket -> Fields -> Bool
canBeValid :: [Field] -> Int -> Bool
canBeValid fields value = any (\f -> Array.inRange (snd' f) value || Array.inRange (trd f) value) fields

fst' (a, _, _) = a

snd' (_, a, _) = a

trd (_, _, a) = a

fieldFitData :: [Int] -> Field -> Bool
fieldFitData values field = all (\x -> Array.inRange (snd' field) x || Array.inRange (trd field) x) values