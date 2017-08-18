module Main where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.Hedgehog

prop_reverse_involutive :: Property
prop_reverse_involutive =
  property $ do
    xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
    reverse (reverse xs) === xs

badReverse :: [a] -> [a]
badReverse [] = []
badReverse [a] = []
badReverse as = reverse as

prop_badReverse_involutive :: Property
prop_badReverse_involutive =
  property $ do
    xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
    badReverse (badReverse xs) === xs

main :: IO ()
main =
  defaultMain $
  testGroup "tasty-hedgehog tests"
    [ testProperty
        "reverse involutive"
        prop_reverse_involutive
    , expectFail $
        testProperty
          "badReverse involutive fails" 
           prop_badReverse_involutive
    ]
