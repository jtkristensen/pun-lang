{-# LANGUAGE DeriveGeneric, ScopedTypeVariables #-}

module BST where

import GHC.Generics
import Test.Tasty.QuickCheck
import Data.List as L (nubBy)
import Data.Function (on)

-- ---------------------------------------------------
-- From 'How to Specify It! ...' by John Hughes
-- Implementation of insert', find and delete is based
-- off the algorithms from IN2010
-- ---------------------------------------------------

type Key  = Int
type Val  = Int
type Tree = BST Int Int

data BST k v = Leaf | Branch (BST k v) k v (BST k v)
    deriving (Eq, Show, Generic)

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (BST k v) where
  arbitrary = do
    kvs <- arbitrary
    return $ foldr (uncurry insert') nil (kvs :: [(k, v)])
  shrink = filter valid . genericShrink

data Equivs k v = BST k v :~=: BST k v deriving Show
instance (Arbitrary k, Arbitrary v, Ord k) => Arbitrary (Equivs k v) where
    arbitrary = do
        kvs  <- L.nubBy ((==) `on` fst) <$> arbitrary
        kvs' <- shuffle kvs
        return (tree kvs :~=: tree kvs')
        where tree = foldr (uncurry insert') nil

-- ----------------------- Operators  -----------------------

(~=) :: Tree -> Tree -> Property
t1 ~= t2 = toList t1 === toList t2

(≡) :: Int -> Int -> Bool
a ≡ b = a == b

(/≡) :: Int -> Int -> Bool
a /≡ b = not (a ≡ b)

-- ------------------ BST operations ------------------------

valid :: Ord k => BST k v -> Bool
valid Leaf = True
valid (Branch l k _v r) =
  valid l && valid r
  && all (< k) (keys l)
  && all (> k) (keys r)

find :: Ord k => k -> BST k v -> Maybe v
find k (Branch left k' v' right)
    | k == k' = Just v'
    | k >  k' = find k right
    | k <  k' = find k left
find _ _ = Nothing

findMin :: Ord k => BST k v -> Maybe k
findMin (Branch Leaf k' _ _) = Just k'
findMin (Branch left _  _ _) = findMin left
findMin _ = Nothing

nil :: BST k v
nil = Leaf

insert' :: Ord k => k -> v -> BST k v -> BST k v
insert' k v Leaf = Branch Leaf k v Leaf
insert' k v (Branch left k' v' right)
    | k == k' = Branch left               k' v  right
    | k <  k' = Branch (insert' k v left) k' v' right
    | k >  k' = Branch left               k' v' (insert' k v right)
insert' k v _  = Branch Leaf k v Leaf

delete :: Ord k => k -> BST k v -> BST k v
delete _ Leaf = Leaf
delete k tree@(Branch left k' v' right)
    | k <  k'    = Branch (delete k left) k' v' right
    | k >  k'    = Branch left k' v' (delete k right)
    | otherwise  = delete' tree

delete' :: Ord k => BST k v -> BST k v
delete' (Branch Leaf _  _  right) = right
delete' (Branch left _  _  Leaf ) = left
delete' (Branch left _  _  right) = do
    case findMin right of
        (Just k) -> 
            case find k right of
                (Just v) -> Branch left k v (delete k right)
                Nothing -> Leaf
        Nothing -> Leaf
delete' _ = Leaf

union :: Ord k => BST k v -> BST k v -> BST k v
union bst1 Leaf = bst1
union Leaf bst2 = bst2
union (Branch l k v r) bst2 = union l (union r (insert' k v bst2))

toList :: BST k v -> [(k, v)]
toList Leaf = []
toList (Branch left k' v' right) = toList left ++ [(k', v')] ++ toList right

keys :: BST k v -> [k]
keys Leaf = []
keys (Branch left k' _ right) = keys left ++ [k'] ++ keys right

deleteKey :: Eq k => k -> [(k, v)] -> [(k, v)]
deleteKey k = filter ((/= k) . fst)

insertions :: Tree -> [(Int, Int)]
insertions Leaf = []
insertions (Branch l k v r) = (k, v) : insertions l ++ insertions r
