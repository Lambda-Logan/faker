module Faker.CreditCardSpec where

import           Faker.CreditCard
import           Helper
import           Test.Hspec

spec :: Spec
spec = do
  describe "visa" $
    deterministicOutput visa

  describe "mastercard" $
    deterministicOutput mastercard

  describe "discover" $
    deterministicOutput discover

  describe "americanExpress" $
    deterministicOutput americanExpress

  describe "dinersClub" $
    deterministicOutput dinersClub

  describe "jcb" $
    deterministicOutput jcb

  describe "switch" $
    deterministicOutput switch

  describe "solo" $
    deterministicOutput solo

  describe "dankort" $
    deterministicOutput dankort

  describe "maestro" $
    deterministicOutput maestro

  describe "forbrugsforeningen" $
    deterministicOutput forbrugsforeningen

  describe "laser" $
    deterministicOutput laser
