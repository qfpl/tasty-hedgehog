-- | This package lets you test Hedgehog properties with tasty.
--
-- Typical usage would look like this:
--
-- @
-- testGroup "tasty-hedgehog tests" [
--    testPropertyNamed "reverse involutive" "prop_reverse_involutive" prop_reverse_involutive
--  , testPropertyNamed "sort idempotent"    "prop_sort_idempotent"    prop_sort_idempotent
--  ]
-- @
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Test.Tasty.Hedgehog (
    testProperty
  , testPropertyNamed
  , fromGroup
  -- * Options you can pass in via tasty
  , HedgehogReplay(..)
  , HedgehogShowReplay(..)
  , HedgehogTestLimit(..)
  , HedgehogDiscardLimit(..)
  , HedgehogShrinkLimit(..)
  , HedgehogShrinkRetries(..)
  ) where

import Data.Maybe (fromMaybe)
import Data.Typeable

import qualified Test.Tasty as T
import qualified Test.Tasty.Providers as T
import Test.Tasty.Options

import Hedgehog
import Hedgehog.Internal.Config (UseColor, detectColor)
import Hedgehog.Internal.Property
import Hedgehog.Internal.Runner as H
import Hedgehog.Internal.Report
import Hedgehog.Internal.Seed as Seed

data HP = HP T.TestName (Maybe PropertyName) Property
  deriving (Typeable)

-- | Create a 'T.TestTree' from a Hedgehog 'Property'.
testProperty :: T.TestName -> Property -> T.TestTree
testProperty name prop = T.singleTest name (HP name Nothing prop)

-- | `testPropertyNamed` @testName propertyName property@ creates a
-- 'T.TestTree' from @property@ using @testName@ as the displayed
-- description for the property. The @propertyName@ is used by Hedgehog
-- when a failure occurs to provide instructions for how to re-run
-- the property and should normally be set to a string representation
-- of the @property@ argument.
--
-- @
-- testPropertyNamed
--  "reverse is involutive"
--  "prop_reverse_involutive"
--  prop_reverse_involutive
-- @
--
-- @since 1.2.0.0
testPropertyNamed :: T.TestName -> PropertyName -> Property -> T.TestTree
testPropertyNamed name propName prop =
  T.singleTest name (HP name (Just propName) prop)

-- | Create a 'T.TestTree' from a Hedgehog 'Group'.
fromGroup :: Group -> T.TestTree
fromGroup group =
    T.testGroup (unGroupName $ groupName group) $
      map mkTestTree (groupProperties group)
  where
    mkTestTree :: (PropertyName, Property) -> T.TestTree
    mkTestTree (propName, prop) = testProperty (unPropertyName propName) prop

-- | The replay token to use for replaying a previous test run
newtype HedgehogReplay = HedgehogReplay (Maybe (Skip, Seed))
  deriving (Typeable)

instance IsOption HedgehogReplay where
  defaultValue = HedgehogReplay Nothing
  parseValue v = HedgehogReplay . Just <$> replay
    -- Reads a replay token in the form "{skip} {seed}"
    where replay = (,) <$> skipDecompress (unwords skip) <*> safeRead (unwords seed)
          (skip, seed) = splitAt 1 $ words v
  optionName = return "hedgehog-replay"
  optionHelp = return "Replay token to use for replaying a previous test run"

-- | If a test case fails, show a replay token for replaying tests
newtype HedgehogShowReplay = HedgehogShowReplay Bool
  deriving (Typeable)

instance IsOption HedgehogShowReplay where
  defaultValue = HedgehogShowReplay True
  parseValue = fmap HedgehogShowReplay . safeRead
  optionName = return "hedgehog-show-replay"
  optionHelp = return "Show a replay token for replaying tests"

-- | The number of successful test cases required before Hedgehog will pass a test
newtype HedgehogTestLimit = HedgehogTestLimit (Maybe TestLimit)
  deriving (Eq, Ord, Show, Typeable)

instance IsOption HedgehogTestLimit where
  defaultValue = HedgehogTestLimit Nothing
  parseValue = fmap (HedgehogTestLimit . Just . TestLimit) . safeRead
  optionName = return "hedgehog-tests"
  optionHelp = return "Number of successful test cases required before Hedgehog will pass a test"

-- | The number of discarded cases allowed before Hedgehog will fail a test
newtype HedgehogDiscardLimit = HedgehogDiscardLimit (Maybe DiscardLimit)
  deriving (Eq, Ord, Show, Typeable)

instance IsOption HedgehogDiscardLimit where
  defaultValue = HedgehogDiscardLimit Nothing
  parseValue = fmap (HedgehogDiscardLimit . Just . DiscardLimit) . safeRead
  optionName = return "hedgehog-discards"
  optionHelp = return "Number of discarded cases allowed before Hedgehog will fail a test"

-- | The number of shrinks allowed before Hedgehog will fail a test
newtype HedgehogShrinkLimit = HedgehogShrinkLimit (Maybe ShrinkLimit)
  deriving (Eq, Ord, Show, Typeable)

instance IsOption HedgehogShrinkLimit where
  defaultValue = HedgehogShrinkLimit Nothing
  parseValue = fmap (HedgehogShrinkLimit . Just . ShrinkLimit) . safeRead
  optionName = return "hedgehog-shrinks"
  optionHelp = return "Number of shrinks allowed before Hedgehog will fail a test"

-- | The number of times to re-run a test during shrinking
newtype HedgehogShrinkRetries = HedgehogShrinkRetries (Maybe ShrinkRetries)
  deriving (Eq, Ord, Show, Typeable)

instance IsOption HedgehogShrinkRetries where
  defaultValue = HedgehogShrinkRetries Nothing
  parseValue = fmap (HedgehogShrinkRetries . Just . ShrinkRetries) . safeRead
  optionName = return "hedgehog-retries"
  optionHelp = return "Number of times to re-run a test during shrinking"

propertyTestLimit :: PropertyConfig -> TestLimit
propertyTestLimit =
  let
    getTestLimit (EarlyTermination _ tests) = tests
    getTestLimit (NoEarlyTermination _ tests) = tests
    getTestLimit (NoConfidenceTermination tests) = tests
  in
    getTestLimit . propertyTerminationCriteria

reportToProgress :: PropertyConfig
                 -> Report Progress
                 -> T.Progress
reportToProgress config Report{
    reportTests = testsDone,
    reportStatus = status
  } =
  let
    TestLimit testLimit = propertyTestLimit config
    ShrinkLimit shrinkLimit = propertyShrinkLimit config
    ratio x y = 1.0 * fromIntegral x / fromIntegral y
  in
    -- TODO add details for tests run / discarded / shrunk
    case status of
      Running ->
        T.Progress "Running" (ratio testsDone testLimit)
      Shrinking fr ->
        T.Progress "Shrinking" (ratio (failureShrinks fr) shrinkLimit)

reportOutput :: Bool
             -> UseColor
             -> T.TestName
             -> Maybe PropertyName
             -> Report Result
             -> IO String
reportOutput showReplay useColor testName name report = do
  s <- renderResult useColor name report
  pure $ case reportStatus report of
    Failed fr ->
      let
        count = reportTests report
        seed = reportSeed report
        replayStr =
          if showReplay
          then
            "\nUse '--pattern \"$NF ~ /" ++
            testName ++
            "/\" --hedgehog-replay \"" ++
            skipCompress (SkipToShrink count $ failureShrinkPath fr) ++
            " " ++
            show seed ++
            "\"' to reproduce from the command-line."
          else ""
      in
        s ++ replayStr ++ "\n"
    _ -> s

instance T.IsTest HP where
  testOptions =
    return [ Option (Proxy :: Proxy HedgehogReplay)
           , Option (Proxy :: Proxy HedgehogShowReplay)
           , Option (Proxy :: Proxy HedgehogTestLimit)
           , Option (Proxy :: Proxy HedgehogDiscardLimit)
           , Option (Proxy :: Proxy HedgehogShrinkLimit)
           , Option (Proxy :: Proxy HedgehogShrinkRetries)
           ]

  run opts (HP testName name (Property pConfig pTest)) yieldProgress = do
    useColor <- detectColor
    let
      HedgehogReplay         replay = lookupOption opts
      HedgehogShowReplay showReplay = lookupOption opts
      HedgehogTestLimit       mTests = lookupOption opts
      HedgehogDiscardLimit mDiscards = lookupOption opts
      HedgehogShrinkLimit   mShrinks = lookupOption opts
      HedgehogShrinkRetries mRetries = lookupOption opts
      config =
        PropertyConfig
          (fromMaybe (propertyDiscardLimit pConfig) mDiscards)
          (fromMaybe (propertyShrinkLimit pConfig) mShrinks)
          (fromMaybe (propertyShrinkRetries pConfig) mRetries)
          (NoConfidenceTermination $ fromMaybe (propertyTestLimit pConfig) mTests)
          (maybe Nothing (Just . fst) replay)

    randSeed <- Seed.random
    let seed = maybe randSeed snd replay

    report <- checkReport config 0 seed pTest (yieldProgress . reportToProgress config)

    let
      resultFn = if reportStatus report == OK
                 then T.testPassed
                 else T.testFailed

    out <- reportOutput showReplay useColor testName name report
    return $ resultFn out
