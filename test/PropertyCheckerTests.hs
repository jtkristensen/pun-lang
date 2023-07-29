module PropertyCheckerTests where

import Syntax
import PropertyChecker
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

generateGenerator_tests :: TestTree
generateGenerator_tests =
  testGroup "GenerateGenerator tests."
    [ testCase "Generate Integer'" $
        -- TODO: check if generatedInt has type Integer?
        do
          generatedValue <- generate $ generateGenerator Integer'
          case (generatedValue) of
            (Number _ Integer') -> True
            _                   -> False
            @?= True
    ]
