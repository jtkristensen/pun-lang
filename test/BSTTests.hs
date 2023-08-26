{-# LANGUAGE DeriveGeneric, ScopedTypeVariables #-}
-- https://stackoverflow.com/questions/43712935/could-not-deduce-bounded-a1-arising-from-a-use-of-minbound

module BSTTests where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
import BST

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (BST k v) where
  arbitrary = do
    kvs <- arbitrary
    return $ foldr (uncurry insert) nil (kvs :: [(k, v)])
  shrink = filter valid . genericShrink

-- ------------------ Validity properties  ------------------
valid :: Ord k => BST k v -> Bool
valid Leaf = True
valid (Branch l k _v r) =
  valid l && valid r
  && all (< k) (keys l)
  && all (> k) (keys r)

prop_NilValid :: Bool
prop_NilValid = valid (nil :: Tree)

prop_InsertValid :: Key -> Val -> Tree -> Bool
prop_InsertValid k v t = valid (insert k v t)

prop_DeleteValid :: Key -> Tree -> Bool
prop_DeleteValid k t = valid (delete k t)

prop_UnionValid :: Tree -> Tree -> Bool
prop_UnionValid t t' = valid (union t t')

prop_ArbitraryValid :: Tree -> Bool
prop_ArbitraryValid t = valid t

prop_ShrinkValid :: Tree -> Property
prop_ShrinkValid t = valid t ==> filter (not . valid) (shrink t) === []

bst_tests :: TestTree
bst_tests =
  testGroup "Binary search tree :"
  [ testProperty "Nil is valid" $
    prop_NilValid,
    testProperty "Insert is valid" $
    prop_InsertValid,
    testProperty "Delete is valid" $
    prop_DeleteValid,
    testProperty "Union is valid" $
    prop_UnionValid
  ]