
module ParserTests (parserTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad (void)

import Syntax
import Parser
import Data.Either

parserTests :: [TestTree]
parserTests =
  [ testCase "Basic numbers" $
    positive term_
      "id (x : nat) : nat = x . main id ." $
      Number 7 ()
  ]




-- * Utility

strip :: Functor f => f a -> f ()
strip = void

positive :: (Functor f, Eq (f ()), Show (f ())) => Parser (f a) -> String -> f () -> Assertion
positive p s a = strip <$> parseString p s @?= return (strip a)

negative :: Functor f => Parser (f a) -> String -> Assertion
negative p s = assertBool "should not parse" $ isLeft $ strip <$> parseString p s
