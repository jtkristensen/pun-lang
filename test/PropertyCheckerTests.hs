module PropertyCheckerTests where

import Syntax
import PropertyChecker
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

generateGenerator_tests :: TestTree
generateGenerator_tests =
  testGroup "GenerateGenerator tests."
    [ testCase "Generate Integer'" $
        -- TODO: check if generatedInt has type Integer?
        case generateGenerator Integer' of
            (Number generatedInt Integer') -> True
            _ -> False
        @= True
    ]
