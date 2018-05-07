module Faker.LoremSpec where

import           Faker.Lorem
import           Helper
import           Prelude     hiding (words)
import           Test.Hspec

spec :: Spec
spec = do
  describe "word" $
    deterministicOutput word

  describe "words" $
    deterministicOutput $ words 2

  describe "character" $
    deterministicOutput character

  describe "characters" $
    deterministicOutput $ characters 2

  describe "sentence" $
    deterministicOutput sentence

  describe "sentences" $
    deterministicOutput $ sentences 2

  describe "paragraph" $
    deterministicOutput paragraph
  describe "paragraphs" $ deterministicOutput $ paragraphs 2
