module Faker.Business
(
  creditCardNumber
, creditCardExpiryDate
, creditCardType
)
where

import Faker.Utils

creditCardNumber :: Faker String
creditCardNumber = randomBusinessWord "credit_card_number"

creditCardExpiryDate :: Faker String
creditCardExpiryDate = randomBusinessWord "credit_card_expiry_date"

creditCardType :: Faker String
creditCardType = randomBusinessWord "credit_card_type"

randomBusinessWord :: String -> Faker String
randomBusinessWord = randomValue "business"
