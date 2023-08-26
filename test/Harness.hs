import Test.Tasty

-- import ParserTests
import PropertyCheckerTests
import BSTTests

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "Pun-lang - Main Test Suite."
    [
      testGroup "Property checker : "
        [ generateGenerator_tests
        ]
      ,
      testGroup "Binary search tree : "
        [ bst_tests
        ]
    ]
