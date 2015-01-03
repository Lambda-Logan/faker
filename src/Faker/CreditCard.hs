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

mastercard :: IO String
mastercard = randomCardNumber "mastercard"

discover :: IO String
discover = randomCardNumber "discover"

americanExpress :: IO String
americanExpress = randomCardNumber "american_express"

dinersClub :: IO String
dinersClub = randomCardNumber "dinersClub"

jcb :: IO String
jcb = randomCardNumber "jcb"

switch :: IO String
switch = randomCardNumber "switch"

solo :: IO String
solo = randomCardNumber "solo"

dankort :: IO String
dankort = randomCardNumber "dankort"

maestro :: IO String
maestro = randomCardNumber "maestro"

forbrugsforeningen :: IO String
forbrugsforeningen = randomCardNumber "forbrugsforeningen"

laser :: IO String
laser = randomCardNumber "laser"

randomCardNumber :: String -> IO String
randomCardNumber attr = randomValue "credit_card" attr >>= evalRegex
