import Test.Tasty

import ParserTests
import GeneratorGeneratorTests
import BSTTests

timeoutSeconds :: Integer -> Timeout
timeoutSeconds = mkTimeout . (* 1000000)

main :: IO ()
main = defaultMain $ localOption (timeoutSeconds 10) tests

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