module Chapter00 where

-- []    :: [a]     | (:)    :: a -> [a]     -> [a]
-- empty :: Set a   | insert :: a -> Set a   -> Set a
-- empty :: Tree a  | insert :: a -> Tree a  -> Tree a
-- empty :: Queue a | push   :: a -> Queue a -> Queue a

-- If we want to create a type class encompassing all of these types, the abstraction is not in the elements.
-- The moving parts here are [], Set, Tree, and Queue.
-- One property that they share is that they require a type argument to turn them into real types.
-- We say that they are TYPE CONSTRUCTORS.
-- In other words, we cannot have a value or parameter of type Set — we need to write eg. Set Int or Set [Int].

class Container c where
  empty  :: c a
  insert :: a -> c a -> c a

instance Container [] where
  empty  = []
  insert = (:)
--  insert x xs = x : xs
--  insert x xs = (:) x xs

-- Alternative: appending new element
-- instance Container [] where
--   empty = []
--   insert x xs = xs ++ [x]

-- data Queue a = Queue { unQueue :: [a] } deriving (Show)
-- A newtype is merely a way to direct the compiler to choose the wanted instance.
-- Wrapping the list into a newtype:
newtype Queue a = Queue { unQueue :: [a] } deriving (Show)

-- We want new elements appended to the end of the list.
-- New datatype is used to choose it.
instance Container Queue where
  empty = Queue []
  insert x (Queue xs) = Queue (xs ++ [x])
  -- or using the field accessor unQueue:
  -- insert x xs = Queue (unQueue xs ++ [x])

-- insertTwice :: a -> [a] -> [a]
-- insertTwice x xs = insert x (insert x xs)

-- insertTwice' :: a -> Queue a -> Queue a
-- insertTwice' x xs = insert x (insert x xs)

-- Compiler chooses the appropriate insert function based on the type:
insertTwice :: (Container c) => a -> c a -> c a
insertTwice x xs = insert x (insert x xs)

intList :: [Int]
intList = [1, 2, 3, 4, 5]

intQueue :: Queue Int
intQueue = Queue [1, 2, 3, 4, 5]

intListTwice :: [Int]
intListTwice  = insertTwice 10 intList  -- prepending 10 twice: [10,10,1,2,3,4,5]

intQueueTwice :: Queue Int
intQueueTwice = insertTwice 10 intQueue -- appending  10 twice: Queue {unQueue = [1,2,3,4,5,10,10]}
