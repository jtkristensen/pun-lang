import Test.Tasty

import ParserTests
import PropertyCheckerTests
import BSTTests

main :: IO ()
main = defaultMain $ localOption (mkTimeout 5000000) tests

tests :: TestTree
tests =
  testGroup "Pun-lang - Main Test Suite."
    [ testGroup "Property checker :"
        [ generateGeneratorTests
        ]
    , testGroup "Binary search tree : "
        [ bst_tests
        ]
    , testGroup "Parser :"
        parserTests
    ]
