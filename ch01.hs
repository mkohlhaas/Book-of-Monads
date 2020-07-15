module Chapter01 where

data Tree a = Leaf a
            | Node (Tree a) (Tree a)
              deriving (Show)

stringTree :: Tree String
stringTree = Node (Leaf "Hello") (Leaf "World")

numberOfLeaves :: Tree a -> Integer
numberOfLeaves (Leaf _) = 1
numberOfLeaves (Node l r) = numberOfLeaves l + numberOfLeaves r

numberOfStrings :: Integer
numberOfStrings = numberOfLeaves stringTree -- 2

-- relabel :: Tree a -> Int -> (Tree (Int, a), Int)
-- relabel (Leaf a)   i = (Leaf (i, a), i + 1)
-- relabel (Node l r) i = let (l', i1) = relabel l i
--                            (r', i2) = relabel r i1
--                        in (Node l' r', i2)

-- relabel :: Tree a -> Int -> (Tree (Int, a), Int)
-- relabel (Leaf a)   = \i -> (Leaf (i,a), i + 1)
-- relabel (Node l r) = relabel l `next` \l' -> 
--                      relabel r `next` \r' ->
--                      pure1 (Node l' r')

relabel :: Tree a -> Int -> (Tree (Int, a), Int)
relabel (Leaf a)   = \i -> (Leaf (i, a), i + 1)
relabel (Node l r) = relabel l >>>= \l -> 
                     relabel r >>>= \r ->
                     pure1 (Node l r)

type WithCounter a = Int -> (a, Int)

next :: WithCounter a -> (a -> WithCounter b) -> WithCounter b
f `next` g = \i -> let (r, i') = f i in g r i'

-- Infix operator looks nicer.
(>>>=) = next

pure1 :: a -> WithCounter a
pure1 a = \i -> (a, i)

treeRelabeled :: Tree (Int, String)
treeRelabeled = fst $ relabel stringTree 1 -- Node (Leaf (1,"Hello")) (Leaf (2,"World"))
