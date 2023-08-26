
module ParserTests (parserTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad (void)

import Syntax
import Parser
import Text.Parsec (eof)
import Data.Either

parserTests :: [TestTree]
parserTests =
  [ testGroup "positive tests for parsing nats"  natParserTests_positive
  , testGroup "negative tests for parsing nats"  natParserTests_negative
  , testGroup "positive tests for parsing ints"  intParserTests_positive
  , testGroup "negative tests for parsing ints"  intParserTests_negative
  , testGroup "positive tests for parsing types" typeParserTests_positive
  , testGroup "negative tests for parsing types" typeParserTests_negative
  ]

natParserTests_positive :: [TestTree]
natParserTests_positive =
  map (\(s, v) -> testCase s $ (nat, s) `parsesTo` v)
  [ ( "0",  0)
  , ("00",  0)
  , ("10", 10)
  ]

natParserTests_negative :: [TestTree]
natParserTests_negative =
  map (\s -> testCase ("'" ++ s ++ "' is not a nat") $ nat `shallNotParse` s)
  [ ""
  , " 00"
  ]

intParserTests_positive :: [TestTree]
intParserTests_positive =
  map (\(s, v) -> testCase s $ (int, s) `parsesTo` v)
  [ (   "0",   0)
  , (  "00",   0)
  , (  "10",  10)
  , ( "~ 5",  -5)
  , ( "~05",  -5)
  , ( "~10", -10)
  , ("~ 42", -42)
  ]

intParserTests_negative :: [TestTree]
intParserTests_negative =
  map (\s -> testCase ("'" ++ s ++ "' is not an int") $ int `shallNotParse` s)
  [ ""
  , "-0"
  , " ~0"
  ]


typeParserTests_positive :: [TestTree]
typeParserTests_positive =
  map (\(s, v) -> testCase s $ (type_, s) `parsesTo` v)
  [ ("0", Variable'  0)
  , ("10", Variable' 10)
  , ("integer", Integer')
  , ("(integer)", Integer')
  , ("((integer))", Integer')
  , ("integer -> boolean", Integer' :->: Boolean')
  , ("integer -> integer -> integer", Integer' :->: (Integer' :->: Integer'))
  , ("(integer -> integer) -> integer", (Integer' :->: Integer') :->: Integer')
  , ("bst integer (boolean -> (0 -> 1))",
      (BST Integer' (Boolean' :->: (Variable' 0 :->: Variable' 1))))
  , ("(boolean,boolean)", Boolean' :*: Boolean')
  , ("(boolean ,boolean)", Boolean' :*: Boolean')
  , ("(boolean, boolean)", Boolean' :*: Boolean')
  , ("( boolean,boolean)", Boolean' :*: Boolean')
  , ("(boolean,boolean )", Boolean' :*: Boolean')
  , ("(integer , integer) -> bst integer integer -> bst integer integer",
      (Integer' :*: Integer') :->: (BST Integer' Integer' :->: BST Integer' Integer'))
  ]

typeParserTests_negative :: [TestTree]
typeParserTests_negative =
  map (\s -> testCase ("(" ++ s ++ ") is not a type.") $ (type_ >> eof) `shallNotParse` s)
  [ ""
  , "integers"
  , "integer*b"
  , "(boolean -> boolean"
  ]

termParserTests_positive :: [TestTree]
termParserTests_positive =
  map (\(s, v) -> positive term_ s v)
  [ ("0", Variable'  0)
  ]

termParserTests_negative :: [TestTree]
termParserTests_negative =
  map (\s -> negative term_ s)
  [ ("0", Variable'  0)
  ]


-- * Dealing with lore in unit-tests.

strip :: Functor f => f a -> f ()
strip = void

parsesTo :: (Show a, Eq a) => (Parser a, String) -> a -> Assertion
parsesTo (p, s) a = parseString p s @?= return a

shallNotParse :: Show a => Parser a -> String -> Assertion
shallNotParse p s = assertBool "shall not parse" $ isLeft $ parseString p s

positive :: (Functor f, Eq (f ()), Show (f ())) => Parser (f a) -> String -> f () -> Assertion
positive p s a = strip <$> parseString p s @?= return (strip a)

negative :: Functor f => Parser (f a) -> String -> Assertion
negative p s = assertBool "should not parse" $ isLeft $ strip <$> parseString p s
