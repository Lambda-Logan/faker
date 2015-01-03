module Faker.CreditCard
(
  visa
, mastercard
, discover
, americanExpress
, dinersClub
, jcb
, switch
, solo
, dankort
, maestro
, forbrugsforeningen
, laser
)
where

import Faker.Utils

visa :: IO String
visa = randomCardNumber "visa"

randomCardNumber :: String -> IO String
randomCardNumber attr = randomValue "credit_card" attr
