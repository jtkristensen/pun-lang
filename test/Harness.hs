import Test.Tasty

import ParserTests
import PropertyCheckerTests
import InterpreterTests

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "Pun-lang - Main Test Suite."
    [
      testGroup "Property checker :"
        [ generateGenerator_tests
        ]
    , testGroup "Parser :"
        parserTests
    , testGroup "Interpreter :"
        interpreter_tests
    ]
