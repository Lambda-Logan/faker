module Faker.BusinessSpec where

import           Faker.Business
import           Helper
import           Test.Hspec

spec :: Spec
spec = do
  describe "creditCardNumber" $
    deterministicOutput creditCardNumber

  describe "creditCardExpiryDate" $
    deterministicOutput creditCardExpiryDate

  describe "creditCardType" $
    deterministicOutput creditCardType
