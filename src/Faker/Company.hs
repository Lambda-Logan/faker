module Faker.Company
(
  buzzwords
, bs
, suffix
, name
, catchPhrase
, ein
, dunsNumber
, logo
)
where

import Faker.Utils
import qualified Faker.Name as N

name :: Faker String
name = do
    ind <- randomNum (0,2)
    name1 <- N.lastName
    name2 <- N.lastName
    name3 <- N.lastName
    suff  <- suffix
    return $ case ind of
               0 -> name1 ++ " " ++ suff
               1 -> name1 ++ "-" ++ name2
               _ -> name1 ++ ", " ++ name2 ++ " and " ++ name3

buzzwords :: Faker String
buzzwords = randomCompanyWord "buzzwords"

bs :: Faker String
bs = randomCompanyWord "bs"

suffix :: Faker String
suffix = randomCompanyWord "suffix"

catchPhrase :: Faker String
catchPhrase = undefined

ein :: Faker String
ein = undefined

dunsNumber :: Faker String
dunsNumber = undefined

logo :: Faker String
logo = undefined

randomCompanyWord :: String -> Faker String
randomCompanyWord = randomValue "company"
