{-# LANGUAGE DeriveGeneric, ScopedTypeVariables #-}
-- https://stackoverflow.com/questions/43712935/could-not-deduce-bounded-a1-arising-from-a-use-of-minbound

module BSTTests where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
-- Control.Applicative for using <|>
import Control.Applicative
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

-- ------------------ Post conditions  ------------------
prop_FindPostPresent :: Key -> Val -> Tree -> Property
prop_FindPostPresent k v t = find k (insert k v t) === Just v

prop_FindPostAbsent :: Key -> Tree -> Property
prop_FindPostAbsent k t = find k (delete k t) === Nothing

prop_InsertDeleteComplete :: Key -> Tree -> Property
prop_InsertDeleteComplete k t =
  case find k t of
    Nothing -> t === delete k t
    Just v  -> t === insert k v t

prop_InsertPost :: Key -> Val -> Tree -> Key -> Property
prop_InsertPost k v t k' =
  find k' (insert k v t) === if k == k' then Just v else find k' t

prop_InsertPostSameKey :: Key -> Val -> Tree -> Property
prop_InsertPostSameKey k v t = prop_InsertPost k v t k

prop_UnionPost :: Tree -> Tree -> Key -> Property
prop_UnionPost t t' k = find k (union t t') === (find k t <|> find k t')

-- ------------------ Metamorphic properties  ------------------
(~) :: Tree -> Tree -> Property
t1 ~ t2 = toList t1 === toList t2

prop_InsertInsert :: (Key, Val) -> (Key, Val) -> Tree -> Property
prop_InsertInsert (k, v) (k', v') t =
  (insert k v (insert k' v' t))
  ~
  (if k == k' then insert k v t else insert k' v' (insert k v t))

prop_InsertDelete :: (Key, Val) -> Key -> Tree -> Property
prop_InsertDelete (k, v) k' t = 
  (insert k v (delete k' t))
  ~
  if k == k' then insert k v t else delete k' (insert k v t)

prop_InsertUnion :: (Key, Val) -> Tree -> Tree -> Property
prop_InsertUnion (k, v) t t' =
  (insert k v (union t t')) ~ union (insert k v t) t'

bst_tests :: TestTree
bst_tests =
  testGroup "Properties: "
  [
    testGroup "Validity properties: "
    [ testProperty "Nil is valid" $
      prop_NilValid,
      testProperty "Insert is valid" $
      prop_InsertValid,
      testProperty "Delete is valid" $
      prop_DeleteValid,
      testProperty "Union is valid" $
      prop_UnionValid
    ],
    testGroup "Post conditions: "
    [ testProperty "Find key after inserting it" $
      prop_FindPostPresent,
      testProperty "Do not find key after deleting it" $
      prop_FindPostAbsent,
      testProperty "Insert delete post condition" $
      prop_InsertDeleteComplete,
      testProperty "Insert post condition" $
      prop_InsertPost,
      testProperty "Insert same key post condition" $
      prop_InsertPostSameKey,
      testProperty "Union post condition" $
      prop_UnionPost
    ],
    testGroup "Metamorphic properties: "
    [ testProperty ("Inserting twice gives same keys and values " ++
                   "regardless of the insertion order ") $
      prop_InsertInsert,
      testProperty "Insert delete"  $
      prop_InsertDelete,
      testProperty "Insert union" $
      prop_InsertUnion
    ]
  ]