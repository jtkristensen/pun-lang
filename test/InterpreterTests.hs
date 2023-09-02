module InterpreterTests where

import Syntax
import Unification
import Interpreter

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

interpreter_tests :: TestTree
interpreter_tests =
  testGroup "'interpret' tests :"
    [
      -- testCase "Legal case statement unifies" $
        
    ]
