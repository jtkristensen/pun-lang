{-# LANGUAGE DeriveGeneric #-}

module BST where

import Test.Tasty.QuickCheck
import GHC.Generics

-- ----------------------------------------------
-- From 'How to Specify It! ...' by John Hughes
-- ----------------------------------------------

-- k : key
-- v : value
data BST k v = Leaf | Branch (BST k v) k v (BST k v)
    deriving (Eq, Show, Generic)

{-
Test:
insert 7 "7" (insert 4 "4" (insert 5 "5" (insert 3 "3" (Branch (Leaf) 1 "1" (Leaf)))))
-}

-- key and value we want to insert into a BST
insert :: Ord k => k -> v -> BST k v -> BST k v
insert k v Leaf = Branch (Leaf) k v (Leaf)
insert k v (Branch left k' v' right)
    | k == k' = Branch left              k' v' right
    | k <  k' = Branch (insert k v left) k' v' right
    | k >  k' = Branch left              k' v' (insert k v right)
insert k v _  = Branch (Leaf) k v (Leaf)

find :: Ord k => k -> BST k v -> Maybe v
find k (Branch left k' v' right)
    | k == k' = Just v'
    | k >  k' = find k right
    | k <  k' = find k left
find k _ = Nothing

findMin :: Ord k => BST k v -> k
findMin (Branch Leaf k' _ _) = k'
findMin (Branch left _  _ _) = findMin left

nil :: BST k v
nil = Leaf

{-
delete 3
Branch Leaf 1 "1" (Branch Leaf 3 "3" (Branch Leaf 5 "5" Leaf))
-}
delete :: Ord k => k -> BST k v -> BST k v
delete _ Leaf = Leaf
delete k tree@(Branch left k' v' right)
    | k <  k'    = Branch (delete k left) k' v' right
    | k >  k'    = Branch left k' v' (delete k right)
    | otherwise  = delete' tree

delete' :: Ord k => BST k v -> BST k v
delete' (Branch Leaf _  _  right) = right
delete' (Branch left _  _  Leaf ) = left
delete' (Branch left k' v' right) = Branch left (findMin right) v' (delete k' right)