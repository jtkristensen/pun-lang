{-# LANGUAGE DeriveGeneric, ScopedTypeVariables #-}
-- https://stackoverflow.com/questions/43712935/could-not-deduce-bounded-a1-arising-from-a-use-of-minbound

module BSTTests where

import Test.Tasty
import Test.Tasty.QuickCheck
import qualified Data.List as List (insert, sort, unionBy, lookup)
import Data.Function (on)
import Control.Applicative ((<|>))
import BST

-- ------------------ Validity properties  ------------------
prop_NilValid :: Bool
prop_NilValid = valid (nil :: Tree)

prop_InsertValid :: Key -> Val -> Tree -> Property
prop_InsertValid k v t = valid t ==> valid (insert' k v t)

prop_DeleteValid :: Key -> Tree -> Property
prop_DeleteValid k t = valid t ==> valid (delete k t)

prop_UnionValid :: Tree -> Tree -> Property
prop_UnionValid t t' = valid t && valid t' ==> valid (union t t')

prop_ArbitraryValid :: Tree -> Bool
prop_ArbitraryValid t = valid t

prop_ShrinkValid :: Tree -> Property
prop_ShrinkValid t = valid t ==> filter (not . valid) (shrink t) === []

validityTests :: TestTree
validityTests = testGroup "\nValidity tests" [
    testProperty "Nil is a valid tree"            prop_NilValid,
    testProperty "Insertion returns a valid tree" prop_InsertValid,
    testProperty "Delete returns a valid tree"    prop_DeleteValid,
    testProperty "Union returns a valid tree"     prop_UnionValid,
    testProperty "Shrinking returns a valid tree" prop_ShrinkValid]

-- ------------------ Post conditions  ------------------
prop_FindPostPresent :: Key -> Val -> Tree -> Property
prop_FindPostPresent k v t = find k (insert' k v t) === Just v

prop_FindPostAbsent :: Key -> Tree -> Property
prop_FindPostAbsent k t = find k (delete k t) === Nothing

prop_InsertDeleteComplete :: Key -> Tree -> Property
prop_InsertDeleteComplete k t =
  case find k t of
    Nothing -> t === delete k t
    Just v  -> t === insert' k v t

prop_InsertPost :: Key -> Val -> Tree -> Key -> Property
prop_InsertPost k v t k' =
  find k' (insert' k v t) === if k ≡ k' then Just v else find k' t

prop_InsertPostSameKey :: Key -> Val -> Tree -> Property
prop_InsertPostSameKey k v t = prop_InsertPost k v t k

prop_UnionPost :: Tree -> Tree -> Key -> Property
prop_UnionPost t t' k = find k (union t t') === (find k t <|> find k t')

postConditions :: TestTree
postConditions = testGroup "\nPost-conditions" [
    testProperty "Insert post-condition: able to find key after insertion"    prop_InsertPost,
    testProperty "Insert post-condition same key"                             prop_InsertPostSameKey,
    testProperty "Union post-condition"                                       prop_UnionPost,
    testProperty "Find post-present: tree contains key after insertion"       prop_FindPostPresent,
    testProperty "Find post-absent: tree does not contain key after deletion" prop_FindPostAbsent,
    testProperty ("Insert delete: deleting a key that is not present or\n" ++
                  "inserting one that is does nothing")                       prop_InsertDeleteComplete]

-- ------------------ Metamorphic properties  ------------------

prop_InsertInsertWeak :: (Key, Val) -> (Key, Val) -> Tree -> Property
prop_InsertInsertWeak (k, v) (k', v') t =
    k /≡ k' ==> insert' k v (insert' k' v' t) ~= insert' k' v' (insert' k v t)

prop_InsertInsert :: (Key, Val) -> (Key, Val) -> Tree -> Property
prop_InsertInsert (k, v) (k', v') t =
    insert' k v (insert' k' v' t)
    ~=
    if k ≡ k' then insert' k v t else insert' k' v' (insert' k v t)

prop_InsertDeleteWeak :: (Key, Val) -> Key -> Tree -> Property
prop_InsertDeleteWeak (k, v) k' t =
    k /= k' ==> insert' k v (delete k' t) ~= delete k' (insert' k v t)

prop_InsertDelete :: (Key, Val) -> Key -> Tree -> Property
prop_InsertDelete (k, v) k' t =
    insert' k v (delete k' t)
    ~=
    if k ≡ k' then insert' k v t else delete k' (insert' k v t)

prop_InsertUnion :: (Key, Val) -> Tree -> Tree -> Property
prop_InsertUnion (k, v) t t' =
    insert' k v (union t t') ~= union (insert' k v t) t'

prop_DeleteInsertWeak :: Key -> (Key, Val) -> Tree -> Property
prop_DeleteInsertWeak k (k', v') t = k /= k' ==>
    delete k (insert' k' v' t) ~= insert' k' v' (delete k t)

prop_DeleteNil :: Key -> Property
prop_DeleteNil k = delete k nil === (nil :: Tree)

prop_DeleteInsert :: Key -> (Key, Val) -> Tree -> Property
prop_DeleteInsert k (k', v') t =
    delete k (insert' k' v' t)
    ~= if k == k' then delete k t else insert' k' v' (delete k t)

prop_DeleteDelete :: Key -> Key -> Tree -> Property
prop_DeleteDelete k k' t = delete k (delete k' t) ~= delete k' (delete k t)

prop_DeleteUnion :: Key -> Tree -> Tree -> Property
prop_DeleteUnion k t t' = delete k (union t t') ~= union (delete k t) (delete k t')

prop_UnionNil2 :: Tree -> Property
prop_UnionNil2 t = union t nil === t

prop_UnionDeleteInsert :: Tree -> Tree -> (Key, Val) -> Property
prop_UnionDeleteInsert t t' (k, v) =
    union (delete k t) (insert' k v t') ~= insert' k v (union t t')

prop_UnionUnionIdem :: Tree -> Property
prop_UnionUnionIdem t = union t t ~= t

prop_UnionUnionAssoc :: Tree -> Tree -> Tree -> Property
prop_UnionUnionAssoc t1 t2 t3 =
    union (union t1 t2) t3 === union t1 (union t2 t3)

prop_FindNil :: Key -> Property
prop_FindNil k = find k (nil :: Tree) === Nothing

prop_FindInsert :: Key -> (Key, Val) -> Tree -> Property
prop_FindInsert k (k', v') t =
    find k (insert' k' v' t) === if k == k' then Just v' else find k t

metamorphicProperties :: TestTree
metamorphicProperties = testGroup "\nMetamorphic properties" [
    testProperty "Insert insert weak"         prop_InsertInsertWeak,
    testProperty "Insert insert"              prop_InsertInsert,
    testProperty "Insert delete weak"         prop_InsertDeleteWeak,
    testProperty "Insert delete"              prop_InsertDelete,
    testProperty "Insert union"               prop_InsertUnion,
    testProperty "Delete insert weak"         prop_DeleteInsertWeak,
    testProperty "Delete nil"                 prop_DeleteNil,
    testProperty "Delete insert"              prop_DeleteInsert,
    testProperty "Delete delete"              prop_DeleteDelete,
    testProperty "Delete union"               prop_DeleteUnion,
    testProperty "Union nil 2"                prop_UnionNil2,
    testProperty "Union delete insert"        prop_UnionDeleteInsert,
    testProperty "Union is idempotent"        prop_UnionUnionIdem,
    testProperty "Union union is associative" prop_UnionUnionAssoc,
    testProperty "Find nil"                   prop_FindNil,
    testProperty "Find insert"                prop_FindInsert]

-- ------------------ Preservation of equivalence  ------------------

prop_InsertPreservesEquiv :: Key -> Val -> Equivs Int Int -> Property
prop_InsertPreservesEquiv k v (t :~=: t') = insert' k v t ~= insert' k v t'

prop_DeletePreservesEquiv :: Key -> Equivs Int Int -> Property
prop_DeletePreservesEquiv k (t :~=: t') = delete k t ~= delete k t'

prop_UnionPreservesEquiv :: Equivs Int Int -> Equivs Int Int -> Property
prop_UnionPreservesEquiv (t1 :~=: t1') (t2 :~=: t2') = union t1 t2 ~= union t1' t2'

prop_FindPreservesEquiv :: Key -> Equivs Int Int -> Property
prop_FindPreservesEquiv k (t :~=: t') = find k t === find k t'

prop_Equivs :: Equivs Int Int -> Property
prop_Equivs (t :~=: t') = t ~= t'

prop_ShrinkEquivs :: Equivs Int Int -> Property
prop_ShrinkEquivs (t :~=: t') =
    t ~= t' ==> all (\(t :~=: t') -> t ~= t') (shrink (t :~=: t'))
    where t ~= t' = toList t == toList t'

preservationOfEquivalence :: TestTree
preservationOfEquivalence = testGroup "\nPreservation of equivalence" [
    testProperty "Insert preserves equivalence" prop_InsertPreservesEquiv,
    testProperty "Delete preserves equivalence" prop_DeletePreservesEquiv,
    testProperty "Union preserves equivalence"  prop_UnionPreservesEquiv,
    testProperty "Find preserves equivalence"   prop_FindPreservesEquiv,
    testProperty "Equivalence" prop_Equivs,
    testProperty "Shrink preserves equivalence" prop_ShrinkEquivs]

-- ------------------ Inductive testing  ------------------

prop_UnionNil1 :: Tree -> Property
prop_UnionNil1 t = union nil t === t

prop_UnionInsert :: Tree -> Tree -> (Key, Val) -> Property
prop_UnionInsert t t' (k, v) = union (insert' k v t) t' ~= insert' k v (union t t')

prop_InsertComplete :: Tree -> Property
prop_InsertComplete t = t === foldl (flip $ uncurry insert') nil (insertions t)

prop_InsertCompleteForDelete :: Key -> Tree -> Property
prop_InsertCompleteForDelete k t = prop_InsertComplete (delete k t)

prop_InsertCompleteForUnion :: Tree -> Tree -> Property
prop_InsertCompleteForUnion t t' = prop_InsertComplete (union t t')

inductiveTesting :: TestTree
inductiveTesting = testGroup "\nInductive testing" [
    testProperty "Union nil 1"                prop_UnionNil1,
    testProperty "Union insert"               prop_UnionInsert,
    testProperty "Insert complete"            prop_InsertComplete,
    testProperty "Insert complete for delete" prop_InsertCompleteForDelete,
    testProperty "Insert complete for union"  prop_InsertCompleteForUnion]

-- ------------------ Model-based properties  ------------------
prop_NilModel :: Property
prop_NilModel = toList (nil :: Tree) === []

prop_InsertModel :: Key -> Val -> Tree -> Property
prop_InsertModel k v t =
    toList (insert' k v t) === List.insert (k, v) (deleteKey k $ toList t)

prop_DeleteModel :: Key -> Tree -> Property
prop_DeleteModel k t = toList (delete k t) === deleteKey k (toList t)

prop_UnionModel :: Tree -> Tree -> Property
prop_UnionModel t t' =
    toList (union t t') === List.sort (List.unionBy ((≡) `on` fst) (toList t) (toList t'))

prop_FindModel :: Key -> Tree -> Property
prop_FindModel k t = find k t === List.lookup k (toList t)

modelBasedProperties :: TestTree
modelBasedProperties = testGroup "\nModel-based properties" [
    testProperty "Nil model"    prop_NilModel,
    testProperty "Insert model" prop_InsertModel,
    testProperty "Delete model" prop_DeleteModel,
    testProperty "Union model"  prop_UnionModel,
    testProperty "Find model"   prop_FindModel]

-- ---------- Coverage of equivalent trees generator ----------

prop_GeneratedEquivTreesDifferInShape :: Equivs Int Int -> Property
prop_GeneratedEquivTreesDifferInShape (t1 :~=: t2) = checkCoverage $
    cover 1 (t1 /= t2) "of generated equivalent trees differ in shape" True

coverageOfEquivTreesGenerator :: TestTree
coverageOfEquivTreesGenerator = testGroup "\nCoverage of equivalent trees generator" [
    testProperty
    "At least 1% of generated equivalent trees differ in shape"
    prop_GeneratedEquivTreesDifferInShape]

-- ---------------------- All properties  ----------------------
bst_tests :: TestTree
bst_tests =
  testGroup "Properties: " [
    validityTests,
    postConditions,
    metamorphicProperties,
    preservationOfEquivalence,
    inductiveTesting,
    modelBasedProperties,
    coverageOfEquivTreesGenerator]
