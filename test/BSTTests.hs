{-# LANGUAGE DeriveGeneric, ScopedTypeVariables #-}
-- https://stackoverflow.com/questions/43712935/could-not-deduce-bounded-a1-arising-from-a-use-of-minbound

module BSTTests where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
-- Data.List for using nubBy
import qualified Data.List as L
-- Data.Function for using the `on` operator
import Data.Function
-- Control.Applicative for using <|>
import Control.Applicative
import BST

-- TODO: make a newtype
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
(~=) :: Tree -> Tree -> Property
t1 ~= t2 = toList t1 === toList t2

prop_InsertInsert :: (Key, Val) -> (Key, Val) -> Tree -> Property
prop_InsertInsert (k, v) (k', v') t =
  (insert k v (insert k' v' t))
  ~=
  (if k == k' then insert k v t else insert k' v' (insert k v t))

prop_InsertDelete :: (Key, Val) -> Key -> Tree -> Property
prop_InsertDelete (k, v) k' t = 
  (insert k v (delete k' t))
  ~=
  if k == k' then insert k v t else delete k' (insert k v t)

prop_InsertUnion :: (Key, Val) -> Tree -> Tree -> Property
prop_InsertUnion (k, v) t t' =
  (insert k v (union t t')) ~= union (insert k v t) t'

-- ------------------ Preservation of equivalence  ------------------
data Equivs k v = (BST k v) :~: (BST k v) deriving Show

instance (Arbitrary k, Arbitrary v, Ord k) => Arbitrary (Equivs k v) where
  arbitrary = do
    kvs  <- L.nubBy ((==) `on` fst) <$> arbitrary
    kvs' <- shuffle kvs
    return (tree kvs :~: tree kvs')
    where tree = foldr (uncurry insert) nil

prop_InsertPreservesEquiv :: Key -> Val -> Equivs Key Val -> Property
prop_InsertPreservesEquiv k v (t :~: t') = (insert k v t) ~= (insert k v t')

prop_DeletePreservesEquiv :: Key -> Equivs Key Val -> Property
prop_DeletePreservesEquiv k (t :~: t') = (delete k t) ~= (delete k t')

prop_UnionPreservesEquiv :: Equivs Key Val -> Equivs Key Val -> Property
prop_UnionPreservesEquiv (t1 :~: t1') (t2 :~: t2') = (union t1 t2) ~= (union t1' t2')

prop_FindPreservesEquiv :: Key -> Equivs Key Val -> Property
prop_FindPreservesEquiv k (t :~: t') = find k t === find k t'

prop_Equivs :: Equivs Key Val -> Property
prop_Equivs (t :~: t') = t ~= t'

-- TODO: check warning where t and t' shadow previous bindings
prop_ShrinkEquivs :: Equivs Key Val -> Property
prop_ShrinkEquivs (t :~: t') =
  t ~= t' ==> all (\(t :~: t') -> t ~= t') (shrink (t :~: t'))
  where t ~= t' = toList t == toList t'

-- ------------------ Inductive testing  ------------------
insertions :: Tree -> [(Key, Val)]
insertions Leaf = []
insertions (Branch l k v r) = (k, v) : insertions l ++ insertions r

prop_InsertComplete :: Tree -> Property
prop_InsertComplete t = t === foldl (flip $ uncurry insert) nil (insertions t)

prop_InsertCompleteForDelete :: Key -> Tree -> Property
prop_InsertCompleteForDelete k t = prop_InsertComplete (delete k t)

prop_InsertCompleteForUnion :: Tree -> Tree -> Property
prop_InsertCompleteForUnion t t' = prop_InsertComplete (union t t')

-- ------------------ Model-based properties  ------------------
deleteKey :: Key -> [(Key, Val)] -> [(Key, Val)]
deleteKey k = filter (((/=) k) . fst)

prop_NilModel :: Property
prop_NilModel = toList (nil :: Tree) === []

prop_InsertModel :: Key -> Val -> Tree -> Property
prop_InsertModel k v t =
  toList (insert k v t) === L.insert (k, v) (deleteKey k $ toList t)

prop_DeleteModel :: Key -> Val -> Tree -> Property
prop_DeleteModel k _ t = toList (delete k t) === deleteKey k (toList t)

prop_UnionModel :: Tree -> Tree -> Property
prop_UnionModel t t' =
  toList (union t t') === L.sort (L.unionBy ((==) `on` fst) (toList t) (toList t'))

prop_FindModel :: Key -> Tree -> Property
prop_FindModel k t = find k t === L.lookup k (toList t)

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
      testProperty "Cannot find key after deleting it" $
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
    [ testProperty ("Insert twice") $
      prop_InsertInsert,
      testProperty "Insert delete"  $
      prop_InsertDelete,
      testProperty "Insert union" $
      prop_InsertUnion
    ],
    testGroup "Preservation of equivalence: "
    [ testProperty "Insert preserves equivalence" $
      prop_InsertPreservesEquiv,
      testProperty "Delete preserves equivalence" $
      prop_DeletePreservesEquiv,
      testProperty "Union preserves equivalence" $
      prop_UnionPreservesEquiv,
      testProperty "Find preserves equivalence" $
      prop_FindPreservesEquiv,
      testProperty "Equivalence" $
      prop_Equivs,
      testProperty "Shrink preserves equivalence" $
      prop_ShrinkEquivs
    ],
    testGroup "Inductive testing: "
    [ testProperty "Insertion complete" $
      prop_InsertComplete,
      testProperty "Insert complete for delete" $
      prop_InsertCompleteForDelete,
      testProperty "Insert complete for union" $
      prop_InsertCompleteForUnion
    ],
    testGroup "Model-based properties: "
    [ testProperty "Nil model" $
      prop_NilModel,
      testProperty "Insert model" $
      prop_InsertModel,
      testProperty "Delete model" $
      prop_DeleteModel,
      testProperty "Union model" $
      prop_UnionModel,
      testProperty "Find model" $
      prop_FindModel
    ]
  ]