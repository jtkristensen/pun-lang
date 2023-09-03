import Test.Tasty

import ParserTests
import PropertyCheckerTests

main :: IO ()
main = defaultMain $ localOption (mkTimeout 5000000) tests

tests :: TestTree
tests =
  testGroup "Pun-lang - Main Test Suite."
    [
      testGroup "Property checker :"
        [ generateGeneratorTests
        ]
    , testGroup "Parser :"
        parserTests
    ]
