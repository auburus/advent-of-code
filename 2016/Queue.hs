module Queue
( Queue
, push
, pop
, length
, queue
, empty
) where

import qualified Data.List as List
import Prelude hiding (length)

data Queue a = Queue [a] [a] deriving (Eq)

instance Show a => Show (Queue a) where
    show (Queue front back) = "Queue " ++ show (front ++ List.reverse back)

-- O(1)
push :: Queue a -> a -> Queue a
push (Queue front back) a = Queue front (a:back)

-- Usually is O(1), O(n) once in a while
pop :: Queue a -> (a, Queue a)
pop (Queue [] [])  = error "Queue is empty"
pop (Queue [] back) = pop (Queue (List.reverse back) [])
pop (Queue (x:front) back) = (x, Queue front back)

-- If lists is O(n), this is O(n) too
length :: Queue a -> Int
length (Queue front back) = List.length front + List.length back

-- Constructs a queue from a list. The first element is at the front of the queue
queue :: [a] -> Queue a
queue xs = Queue xs []

-- Constructs an empty queue
empty :: Queue a
empty = queue []
