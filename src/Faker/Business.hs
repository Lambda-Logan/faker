{-|
Module        : Faker.Business
Description   : Module for generating fake data related to business
Copyright     : (c) Alexey Gaziev, 2014-2018
License       : MIT
Maintainer    : alex.gaziev@gmail.com
Stability     : experimental
Portability   : POSIX

Fake data
-}
module Faker.Business
  (
  -- * Functions for generate fake data related to business
    creditCardNumber
  , creditCardExpiryDate
  , creditCardType
  )
  where

import           Faker.Utils

-- | Returns random business-like credit card number, i.e. "1234-2121-1221-1211"
creditCardNumber :: Faker String
creditCardNumber = randomBusinessWord "credit_card_number"

-- | Returns random business-like credit card expiry date, i.e. "2011-10-12"
creditCardExpiryDate :: Faker String
creditCardExpiryDate = randomBusinessWord "credit_card_expiry_date"

-- | Returns random business-like credit card type, i.e. "visa"
creditCardType :: Faker String
creditCardType = randomBusinessWord "credit_card_type"

randomBusinessWord :: String -> Faker String
randomBusinessWord = randomValue "business"
