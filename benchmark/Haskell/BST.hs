{-# LANGUAGE DeriveGeneric #-}

module BST where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
import Test.QuickCheck
import GHC.Generics

-- ---------------------------------------------------
-- From 'How to Specify It! ...' by John Hughes
-- Implementation of insert, find and delete is based
-- off the algorithms from IN2010
-- ---------------------------------------------------

data BST k v = Leaf | Branch (BST k v) k v (BST k v)
    deriving (Eq, Show, Generic)

type Key = Int
type Val = Int
type Tree = BST Int Int

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

insert :: Ord k => k -> v -> BST k v -> BST k v
insert k v Leaf = Branch (Leaf) k v (Leaf)
insert k v (Branch left k' v' right)
    | k == k' = Branch left              k' v' right
    | k <  k' = Branch (insert k v left) k' v' right
    | k >  k' = Branch left              k' v' (insert k v right)
insert k v _  = Branch (Leaf) k v (Leaf)

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

union :: Ord k => BST k v -> BST k v -> BST k v
union bst1 Leaf = bst1
union bst1 bst2@(Branch left k' v' right) = do
    let bst1' = insert k' v' bst1
    let bst2' = delete k'    bst2
    union bst1' bst2'

toList :: BST k v -> [(k, v)]
toList Leaf = []
toList (Branch left k' v' right) = (toList left) ++ [(k', v')] ++ (toList right)

keys :: BST k v -> [k]
keys Leaf = []
keys (Branch left k' v' right) = (keys left) ++ [k'] ++ (keys right)

insertionExample = insert 7 "7" (insert 4 "4" (insert 5 "5" (insert 3 "3" (Branch (Leaf) 1 "1" (Leaf)))))
treeExample = Branch Leaf 1 "1" (Branch Leaf 3 "3" (Branch Leaf 5 "5" Leaf))

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (BST k v) where
    arbitrary = do
        kvs <- arbitrary
        return $ foldr (uncurry insert) nil (kvs :: [(k, v)])
    shrink = genericShrink

valid Leaf = True
valid (Branch l k _v r) =
    valid l && valid r &&
    all (< k) (keys l) && all (> k) (keys r)

-- Validity properties
prop_NilValid             = valid (nil :: Tree    )
prop_InsertValid k v t    = valid (insert k v t   )
prop_DeleteValid k   t    = valid (delete k   t   )
prop_UnionValid      t t' = valid (union      t t')