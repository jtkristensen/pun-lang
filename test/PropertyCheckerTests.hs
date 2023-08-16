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

-- Todo, do something better than the empty substitution in : generateGenerator <mempty>.

{-
Generated input that made the second test case fail:

Rec "a" (Rec "i" (Rec "e" (Variable "a" (Variable' 0)) (Variable' 0)) (Variable' 0)) (Variable' 0)
Had the type Variable' 0 but we expected the type Boolean'
-}

generateGenerator_tests :: TestTree
generateGenerator_tests =
  testGroup "`generateGenerator` tests :"
    [ testCase "Pairs and Functions are not primitives" $
      do subs           <- generate $ generateSubstitution
         generatedValue <- generate $ oneof $ generateGenerator (subs, []) <$> [Integer', Boolean']
         case generatedValue of
           (Pair    _ _ _) -> False
           (Lambda  _ _ _) -> False
           _               -> True
           @? (show generatedValue ++ " should have been a primitive {^_^}")
    -- Todo: "now, it is `generateGenerator s t`, and we need to say something in the paper about `s`.
    , testCase "generateGenerator t has type Term t forall t." $
      do subs <- generate $ generateSubstitution
         t    <- generate $ generateType subs
         let canonT = refine subs t
         term <- generate $ generateGenerator (subs, []) canonT
         let i           = (maximum $ map fst subs)
         let (t', _, cs) = infer term i
         let subs'       = bindings cs
         let typeOfT'    = annotation (refine subs' <$> t')
         (canonT == typeOfT')
           @? ( "the term " ++ show t' ++
                " had the type " ++ show typeOfT' ++
                " but we expected the type " ++ show canonT
              )
      -- Todo: move this, create better test case
      , testCase "Resolve resolves 'chains' of variables" $
        resolve 2 [(0, Integer'), (1, Variable' 2), (2, Boolean')] @?= Boolean'
    ]

-- Todo, we want to use more complicated types.
-- Todo, should this actually live in `PropertyChecker`?
-- Todo, Where do we get the index for Variable' from?
aType :: Gen Type
aType =
  oneof $
    [ return Integer'
    , return Boolean'
    , ( :*:  ) <$> aType <*> aType
    -- , ( :->: ) <$> aType <*> aType
    -- What do we do about variable here ??
    ]

