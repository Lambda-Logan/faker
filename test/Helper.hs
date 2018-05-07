{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Helper where

import           Faker.Utils
import           System.Random
import           Test.Hspec

deterministicOutput :: (HasCallStack, Eq a, Show a) => Faker a -> Spec
deterministicOutput action =
  it "returns the same output for the same seed" $ do
    seed <- randomIO
    output1 <- runFakerWithSeed seed action
    output2 <- runFakerWithSeed seed action
    output1 `shouldBe` output2
