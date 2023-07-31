module PropertyCheckerTests where

import Syntax
import PropertyChecker
-- import TypeInference
-- import Examples

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

-- TODO! Any way to get this to work, to abstract away common code?
-- testGenOK :: Term Type -> TestTree
-- testGenOK termType = testCase ("Generate " ++ show typeName) $
--   do
--     generatedValue <- generate $ generateGenerator typeName
--     case (getTerm generatedValue, getType generatedValue) of
--       (termName, typeName) -> True
--       _                    -> False
--   where
--     termName = getTerm termType
--     typeName = getType termType

generateGenerator_tests :: TestTree
generateGenerator_tests =
  testGroup "`generateGenerator` tests :"
    [ testCase "Generate Integer'" $
        -- TODO: check if generatedInt has type Integer?
        do
          generatedValue <- generate $ generateGenerator Integer'
          case generatedValue of
            (Number _ Integer') -> True
            _                   -> False
            @?= True
    , testCase "Generate Boolean'" $
        do
          generatedValue <- generate $ generateGenerator Boolean'
          case generatedValue of
            (Boolean _ Boolean') -> True
            _                    -> False
            @?= True
    , testCase "Generate (Integer' :*: Integer')" $
        do
          generatedValue <- generate $ generateGenerator (Integer' :*: Integer')
          case generatedValue of
            (Pair _ _ (Integer' :*: Integer')) -> True
            _                                  -> False
            @?= True
    , testCase "Generate (Integer' :*: Boolean')" $
        do
          generatedValue <- generate $ generateGenerator (Integer' :*: Boolean')
          case generatedValue of
            (Pair _ _ (Integer' :*: Boolean')) -> True
            _                                  -> False
            @?= True
    , testCase "Generate (Integer' :*: (Integer' :*: Integer'))" $
        do
          generatedValue <- generate $
            generateGenerator (Integer' :*: (Integer' :*: Integer'))
          case generatedValue of
            (Pair _ _ (Integer' :*: (Integer' :*: Integer'))) -> True
            _                                                 -> False
            @?= True
    , testCase "Generate (Integer' :->: Integer')" $
        do
          generatedValue <- generate $ generateGenerator (Integer' :->: Integer')
          case generatedValue of
            (Application _ _ (Integer' :->: Integer')) -> True
            _                                          -> False
            @?= True
    , testCase "Generate (Boolean' :->: Integer')" $
        do
          generatedValue <- generate $ generateGenerator (Boolean' :->: Integer')
          case generatedValue of
            (Application _ _ (Boolean' :->: Integer')) -> True
            _                                          -> False
            @?= True
    , testCase "Generate (Integer' :->: (Integer' :->: Integer'))" $
        do
          generatedValue <- generate $
            generateGenerator (Integer' :->: (Integer' :->: Integer'))
          case generatedValue of
            (Application _ _ (Integer' :->: (Integer' :->: Integer'))) -> True
            _                                                          -> False
            @?= True
    ]

