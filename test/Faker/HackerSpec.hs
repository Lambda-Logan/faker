module Faker.HackerSpec where

import           Faker.Hacker
import           Helper
import           Test.Hspec

spec :: Spec
spec = do
  describe "saySomethingSmart" $
    deterministicOutput saySomethingSmart

  describe "abbreviation" $
    deterministicOutput abbreviation

  describe "adjective" $
    deterministicOutput adjective

  describe "noun" $
    deterministicOutput noun

  describe "verb" $
    deterministicOutput verb

  describe "ingverb" $
    deterministicOutput ingverb
