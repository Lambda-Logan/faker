module Faker.AddressSpec where

import           Faker.Address
import           Faker.Utils
import           Test.Hspec

spec :: Spec
spec =
  describe "countryCode" $
    it "returns a code of length 2" $ do
      cc <- runFaker countryCode
      length cc `shouldBe` 2
