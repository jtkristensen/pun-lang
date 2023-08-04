module PropertyCheckerTests where

import Syntax
import PropertyChecker
-- import TypeInference
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
        do
          generatedValue <- generate $ oneof $ generateGenerator <$> [Integer', Boolean']
          case generatedValue of
            (Pair    _ _ _) -> False
            (Lambda  _ _ _) -> False
            _               -> True
            @? (show generatedValue ++ " should have been a primitive {^_^}")
    ]
