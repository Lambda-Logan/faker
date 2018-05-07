module Faker.AddressSpec where

import           Faker.Address
import           Faker.Utils
import           Helper
import           Test.Hspec

spec :: Spec
spec = do
  describe "city" $
    deterministicOutput city

  describe "streetName" $
    deterministicOutput streetName

  describe "streetAddress" $
    deterministicOutput streetAddress

  describe "latitude" $
    deterministicOutput latitude

  describe "longitude" $
    deterministicOutput longitude

  describe "cityPrefix" $
    deterministicOutput cityPrefix

  describe "citySuffix" $
    deterministicOutput citySuffix

  describe "country" $
    deterministicOutput country

  describe "countryCode" $ do
    deterministicOutput countryCode

    it "returns a code of length 2" $ do
      cc <- runFaker countryCode
      length cc `shouldBe` 2

  describe "buildingNumber" $
    deterministicOutput buildingNumber

  describe "streetSuffix" $
    deterministicOutput streetSuffix

  describe "secondaryAddress" $
    deterministicOutput secondaryAddress

  describe "postcode" $
    deterministicOutput postcode

  describe "postcodeByState" $
    deterministicOutput postcodeByState

  describe "state" $
    deterministicOutput state

  describe "stateAbbr" $
    deterministicOutput stateAbbr

  describe "timeZone" $
    deterministicOutput timeZone

  describe "defaultCountry" $
    deterministicOutput defaultCountry
