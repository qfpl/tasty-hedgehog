
import Test.Tasty

import Test.Example.Basic (basicTests)
import Test.Example.Resource (resourceTests)
import Test.Example.STLC (stlcTests)

tests :: TestTree
tests =
  testGroup "tests"
    [ basicTests
    , resourceTests
    , stlcTests
    ]

main :: IO ()
main = defaultMain tests
