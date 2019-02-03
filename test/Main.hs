module Main where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.Hedgehog

genAlphaList :: Gen String
genAlphaList =
  Gen.list (Range.linear 0 100) Gen.alpha

test_involutive :: (MonadTest m, Eq a, Show a) => (a -> a) -> a -> m ()
test_involutive f x =
  f (f x) === x

prop_reverse_involutive :: Property
prop_reverse_involutive =
  property $ do
    xs <- forAll genAlphaList
    test_involutive reverse xs

badReverse :: [a] -> [a]
badReverse [] = []
badReverse [_] = []
badReverse (x : xs) = badReverse xs ++ [x]

prop_badReverse_involutive :: Property
prop_badReverse_involutive =
  property $ do
    xs <- forAll genAlphaList
    test_involutive badReverse xs

main :: IO ()
main =
  defaultMain $
  testGroup "tasty-hedgehog tests"
    [ testProperty
        "reverse involutive"
        prop_reverse_involutive
    , expectFail $ testProperty
        "badReverse involutive fails"
         prop_badReverse_involutive
    ]
