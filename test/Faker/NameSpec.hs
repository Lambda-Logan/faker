module Faker.NameSpec where

import           Faker.Name
import           Helper
import           Test.Hspec


spec :: Spec
spec = do
  describe "firstName" $
    deterministicOutput firstName

  describe "lastName" $
    deterministicOutput lastName

  describe "prefix" $
    deterministicOutput prefix

  describe "suffix" $
    deterministicOutput suffix

  describe "name" $
    deterministicOutput name

  describe "title" $
    deterministicOutput title
