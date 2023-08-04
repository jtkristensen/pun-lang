module PropertyCheckerTests where

import Syntax
import PropertyChecker
import TypeInference
-- import Examples

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

-- Is there a more general test case, that relies on something that commutes
-- with `infer` (Joachim)?

generateGenerator_tests :: TestTree
generateGenerator_tests =
  testGroup "`generateGenerator` tests :"
    [ testCase "Pairs and Functions are not primitives" $
      do generatedValue <- generate $ oneof $ generateGenerator <$> [Integer', Boolean']
         case generatedValue of
           (Pair    _ _ _) -> False
           (Lambda  _ _ _) -> False
           _               -> True
           @? (show generatedValue ++ " should have been a primitive {^_^}")
    , testCase "generateGenerator t has type Term t forall t." $
      do t    <- generate aType
         term <- generate $ generateGenerator t
         let (t', _, cs) = infer term 0
         let subs        = bindings cs
         let typeOfT'    = annotation (refine subs <$> t')
         (t == typeOfT')
           @? ( "the term " ++ show t' ++
                " had the type " ++ show typeOfT' ++
                " but we expected the type " ++ show t
              )
    ]

-- Todo, we want to use more complicated types.
-- Todo, should this actually live in `PropertyChecker`?
-- Todo, Where do we get the index for Variable' from?
aType :: Gen Type
aType =
  oneof $
    [ return Integer'
    , return Boolean'
    -- , ( :*:  ) <$> aType <*> aType
    -- , ( :->: ) <$> aType <*> aType
    -- What do we do about variable here ??
    ]
