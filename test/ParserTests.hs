
module ParserTests (parserTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad (void)

import Syntax
import Parser
import Text.Parsec (eof, runParser)
import Data.Either

parserTests :: [TestTree]
parserTests =
  [ testGroup "positive tests for parsing nats"  natParserTests_positive
  , testGroup "negative tests for parsing nats"  natParserTests_negative
  , testGroup "positive tests for parsing ints"  intParserTests_positive
  , testGroup "negative tests for parsing ints"  intParserTests_negative
  , testGroup "positive tests for parsing types" typeParserTests_positive
  , testGroup "negative tests for parsing types" typeParserTests_negative
  , testGroup "positive tests for parsing terms" termParserTests_positive
  , testGroup "negative tests for parsing terms" termParserTests_negative
  , testGroup "example files from repository   " parseProgramsFromFiles
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
  map (\(s, v) -> testCase s $ positive term_ s v)
  [ ("0", Number 0 ())
  , ("leaf", Leaf ())
  , ("5 5", Application (Number 5 ()) (Number 5 ()) ())
  , ("f x", Application (Variable "f" ()) (Variable "x" ()) ())
  , ("f(x)", Application (Variable "f" ()) (Variable "x" ()) ())
  , ("2+2", Plus (Number 2 ()) (Number 2 ()) ())
  , ("0 <= 1", Leq (Number 0 ()) (Number 1 ()) ())
  , ("if 5 <= 7 then 0 else true",
     If (Leq (Number 5 ()) (Number 7 ()) ()) (Number 0 ()) (Boolean True ()) ()
    )
  , ("[node leaf 3 5 leaf]",
       Node (Leaf ()) (Number 3 ()) (Number 5 ()) (Leaf ()) ())
  , ("[node [ node leaf 3 5 leaf] 4 (true, ~4) leaf]",
      Node
        (Node (Leaf ()) (Number 3 ()) (Number 5 ()) (Leaf ()) ())
        (Number 4 ())
        (Pair (Boolean True ()) (Number (-4) ()) ())
        (Leaf ()) ())
  , ("\\x -> \\f -> \\y -> f x y"
    , (Lambda "x"
         (Lambda "f"
            (Lambda "y"
               (Application
                  (Application (Variable "f" ()) (Variable "x" ()) ())
                (Variable "y" ()) ()) ()) ()) ()
      )
    )
  , ("case t of"   ++
     "; leaf -> 5" ++
     "; [node l1 true true [node l2 false false r2]] -> f l1 l2 r2"
    , Case (Variable "t" ())
      -- leaf -->
        (Number 5 ())
      (Node
         (Variable "l1" ())
         (Boolean True ())
         (Boolean True ())
         (Node (Variable "l2" ()) (Boolean False ()) (Boolean False ()) (Variable "r2" ()) ()) () ,
      -- node -->
       Application
         (Application
            (Application (Variable "f" ()) (Variable "l1" ()) ())
               (Variable "l2" ()) ())
         (Variable "r2" ()) ()) ())
  ]

termParserTests_negative :: [TestTree]
termParserTests_negative =
  map (\s -> testCase ("'" ++ s ++ "'") $ negative term_ s)
  [ ""
  , "(true , )"
  , "[node leaf]"
  , " false"
  ]

parseProgramsFromFiles :: [TestTree]
parseProgramsFromFiles =
  map (\(s, p) ->
         testCase s $
         do src <- readFile s
            let ast = strip <$> runParser program_ () s src
            assertEqual "" (return p) ast
      )
  [ ("examples/nil.pun",
     Declaration "nil" (BST Integer' Integer') $
     Definition  "nil" (Leaf ())               $
     EndOfProgram)
  , ("examples/addition-is-commutative.pun",
     Declaration "equal" (Integer' :->: (Integer' :->: Boolean')) $
     Definition "equal" (Lambda "m" (Lambda "n"
      (If (Leq (Variable "m" ()) (Variable "n" ()) ())
          (Leq (Variable "n" ()) (Variable "m" ()) ())
          (Boolean False ()) ()) ()) ()) $
     Declaration "add" (Integer' :->: (Integer' :->: Integer')) $
     Definition  "add" (Lambda "m" (Lambda "n"
      (Plus (Variable "m" ()) (Variable "n" ()) ()) ()) ()) $
     Property "add-is-commutative" [("m",()),("n",())]
       (Application (Application (Variable "equal" ())
            (Application (Application (Variable "add" ()) (Variable "m" ()) ()) (Variable "n" ()) ()) ())
            (Application (Application (Variable "add" ()) (Variable "m" ()) ()) (Variable "n" ()) ()) ())
     EndOfProgram)
    , ("examples/comparison-and-logical-operators.pun",
       Declaration "equal" (Integer' :->: (Integer' :->: Boolean')) $
       Definition "equal" (Lambda "m" (Lambda "n"
        (If (Leq (Variable "m" ()) (Variable "n" ()) ())
            (Leq (Variable "n" ()) (Variable "m" ()) ())
            (Boolean False ()) ()) ()) ()) $
       Declaration "not" (Boolean' :->: Boolean') $
       Definition  "not" (Lambda "b" 
         (If (Variable "b"  ())
             (Boolean False ())
             (Boolean True  ()) ()) ()) $
       Declaration "and" (Boolean' :->: (Boolean' :->: Boolean')) $
       Definition  "and" (Lambda "b1" (Lambda "b2" 
        (If (Variable "b1" ())
            (Variable "b2" ())
            (Boolean False ()) ()) ()) ()) $
       Declaration "less" (Integer' :->: (Integer' :->: Boolean')) $
       Definition  "less" (Lambda "m" (Lambda "n"
       (Application
        (Application
          (Variable "and" ())
          (Leq (Variable "m" ()) (Variable "n" ()) ()) ())
        (Application 
          (Variable "not" ())
          (Application
            (Application (Variable "equal" ()) (Variable "m" ()) ())
            (Variable "n" ()) ()) ()) ()) ()) ()) $
       EndOfProgram)
    , ("examples/insert.pun",
        Declaration "equal" (Integer' :->: (Integer' :->: Boolean')) $
        Definition "equal" (Lambda "m" (Lambda "n"
        (If (Leq (Variable "m" ()) (Variable "n" ()) ())
            (Leq (Variable "n" ()) (Variable "m" ()) ())
            (Boolean False ()) ()) ()) ()) $
        Declaration "not" (Boolean' :->: Boolean') $
        Definition  "not" (Lambda "b" 
          (If (Variable "b"  ())
              (Boolean False ())
              (Boolean True  ()) ()) ()) $
        Declaration "and" (Boolean' :->: (Boolean' :->: Boolean')) $
        Definition  "and" (Lambda "b1" (Lambda "b2" 
        (If (Variable "b1" ())
            (Variable "b2" ())
            (Boolean False ()) ()) ()) ()) $
        Declaration "less" (Integer' :->: (Integer' :->: Boolean')) $
        Definition  "less" (Lambda "m" (Lambda "n"
        (Application
        (Application
          (Variable "and" ())
          (Leq (Variable "m" ()) (Variable "n" ()) ()) ())
        (Application 
          (Variable "not" ())
          (Application
            (Application (Variable "equal" ()) (Variable "m" ()) ())
            (Variable "n" ()) ()) ()) ()) ()) ()) $
        Declaration "insert" (Integer' :->: (Integer' :->: (BST Integer' Integer' :->: BST Integer' Integer'))) $
        Definition  "insert" (Lambda "k1" (Lambda "v1" (Lambda "t"
        (Case
          (Variable "t" ())
          (Node (Leaf ()) (Variable "k1" ()) (Variable "v1" ()) (Leaf ()) ())
          (Node (Variable "l" ()) (Variable "k2" ()) (Variable "v2" ()) (Variable "r" ()) (),
          If (Application (Application (Variable "equal" ()) (Variable "k1" ()) ()) (Variable "k2" ()) ())
             (Node (Variable "l" ()) (Variable "k2" ()) (Variable "v1" ()) (Variable "r" ()) ())
             (If (Leq (Variable "k1" ()) (Variable "k2" ()) ())
                 (Node (Application
                          (Application
                            (Application (Variable "insert" ()) (Variable "k1" ()) ())
                            (Variable "v1" ()) ())
                          (Variable "l" ()) ())
                      (Variable "k2" ())
                      (Variable "v2" ())
                      (Variable "r" ()) ())
                  (If (Application
                      (Application (Variable "larger" ()) (Variable "k1" ()) ())
                      (Variable "k2" ()) ())
                    (Node (Variable "l" ()) (Variable "k2" ()) (Variable "v2" ())
                          (Application
                            (Application
                              (Application (Variable "insert" ()) (Variable "k1" ()) ())
                              (Variable "v1" ()) ())
                            (Variable "r" ()) ()) ())
                    (Node (Leaf ()) (Variable "k1" ()) (Variable "v1" ()) (Leaf ()) ()) ()) ()) ()) ()) ()) ()) ()) $
       EndOfProgram)
  ]

-- * Dealing with lore in unit-tests.

strip :: Functor f => f a -> f ()
strip = void

parsesTo :: (Show a, Eq a) => (Parser a, String) -> a -> Assertion
parsesTo (p, s) a = parseString p s @?= return a

shallNotParse :: Show a => Parser a -> String -> Assertion
shallNotParse p s = assertBool "shall not parse" $ isLeft $ parseString p s

positive :: (Functor f, Eq (f ()), Show (f ())) => Parser (f a) -> String -> f () -> Assertion
positive p s a = strip <$> parseString p s @?= return a

negative :: Functor f => Parser (f a) -> String -> Assertion
negative p s = assertBool "should not parse" $ isLeft $ strip <$> parseString p s
