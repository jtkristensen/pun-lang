module BSTTests where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
import BST

bst_tests :: TestTree
bst_tests =
  testGroup "Binary search tree :"
  [ testProperty "First test" $
    True ]