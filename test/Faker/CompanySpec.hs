module Faker.CompanySpec where

import           Faker.Company
import           Helper
import           Test.Hspec

spec :: Spec
spec = do
  describe "buzzwords" $
    deterministicOutput buzzwords

  describe "bs" $
    deterministicOutput bs

  describe "suffix" $
    deterministicOutput suffix

  describe "name" $
    deterministicOutput name

  describe "catchPhrase" $
    deterministicOutput catchPhrase

  describe "ein" $
    deterministicOutput ein

  describe "dunsNumber" $
    deterministicOutput dunsNumber

  describe "logo" $
    deterministicOutput logo
