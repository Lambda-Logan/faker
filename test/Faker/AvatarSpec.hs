module Faker.AvatarSpec where

import           Faker.Avatar
import           Helper
import           Test.Hspec

spec :: Spec
spec =
  describe "image" $
    deterministicOutput image
