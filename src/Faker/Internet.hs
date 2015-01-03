module Faker.Internet
(
  freeEmail
, domainSuffix
)
where

import Faker.Utils

freeEmail :: IO String
freeEmail = randomInternetWord "free_email"

domainSuffix :: IO String
domainSuffix = randomInternetWord "domain_suffix"

randomInternetWord :: String -> IO String
randomInternetWord attr = randomValue "internet" attr
