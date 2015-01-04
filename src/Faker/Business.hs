module Faker.Business
(
  creditCardNumber
, creditCardExpiryDate
, creditCardType
)
where

import Faker.Utils

creditCardNumber :: IO String
creditCardNumber = randomBusinessWord "credit_card_number"

creditCardExpiryDate :: IO String
creditCardExpiryDate = randomBusinessWord "credit_card_expiry_date"

creditCardType :: IO String
creditCardType = randomBusinessWord "credit_card_type"

randomBusinessWord :: String -> IO String
randomBusinessWord attr = randomValue "business" attr
