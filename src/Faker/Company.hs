module Faker.Company
(
  buzzwords1
, buzzwords2
, buzzwords3
, bs1
, bs2
, bs3
, suffix
)
where

import Faker.Utils

buzzwords1 :: IO String
buzzwords1 = randomCompanyWord "buzzwords1"

buzzwords2 :: IO String
buzzwords2 = randomCompanyWord "buzzwords2"

buzzwords3 :: IO String
buzzwords3 = randomCompanyWord "buzzwords3"

bs1 :: IO String
bs1 = randomCompanyWord "bs1"

bs2 :: IO String
bs2 = randomCompanyWord "bs2"

bs3 :: IO String
bs3 = randomCompanyWord "bs3"

suffix :: IO String
suffix = randomCompanyWord "suffix"

randomCompanyWord :: String -> IO String
randomCompanyWord attr = randomValue "company" attr
