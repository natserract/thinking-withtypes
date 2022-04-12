module Other.Lib (
  print_tree,
) where

-- Binary search tree
-- insert 15320 => 
{-
  Input: 15320
      1
    0   5
       3 
      2
-}

data Tree a = Nil | Node a (Tree a) (Tree a) deriving (Show)

insert :: Int -> Tree Int -> Tree Int
insert _ Nil = Nil
insert a (Node _ _ _ ) = Nil
insert a (Node root l r) 
  | a == root = Node root l r
  | a < root = Node root (insert root l) r
  | a > root = Node root l (insert root r)

-- data Tree = Nil | Node Int Tree Tree deriving (Show)

-- tree :: Tree -> Int
-- tree Nil = 0
-- tree (Node _ l r) =
--     1 + max (tree l) (tree r)

-- print_tree :: Tree Int -> Tree Int
sum' :: [Int] -> Int
sum' = sum
print_tree = do
  sum' [6, 7]
  -- insert $ 2 (Node (Node  2 Nil Nil) Nil)
  -- print $ tree $ Node 0 (Node 2 Nil Nil) (Node 1 Nil Nil)