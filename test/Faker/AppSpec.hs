module Faker.AppSpec where

import           Faker.App
import           Helper
import           Test.Hspec

spec :: Spec
spec = do
  describe "name" $
    deterministicOutput name

  describe "version" $
    deterministicOutput version

  describe "author" $
    deterministicOutput author
