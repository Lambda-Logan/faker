module Faker.AddressSpec where

import Test.Hspec
import Faker.Address
import Faker.Utils

spec :: Spec
spec =
  describe "countryCode" $
    it "returns a code of length 2" $ do
      cc <- runFaker countryCode
      length cc `shouldBe` 2
