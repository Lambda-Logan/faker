module Faker.InternetSpec where

import           Faker.Internet
import           Helper
import           Test.Hspec

spec :: Spec
spec = do
  describe "freeEmailDomain" $
    deterministicOutput freeEmailDomain

  describe "domainSuffix" $
    deterministicOutput domainSuffix

  describe "email" $
    deterministicOutput email

  describe "freeEmail" $
    deterministicOutput freeEmail

  describe "safeEmail" $
    deterministicOutput safeEmail

  describe "userName" $
    deterministicOutput userName
