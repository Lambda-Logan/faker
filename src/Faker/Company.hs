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

name :: IO String
name = do
    ind <- randomNum (0,2)
    name1 <- N.lastName
    name2 <- N.lastName
    name3 <- N.lastName
    suff  <- suffix
    case ind of
      0         -> return $ name1 ++ " " ++ suff
      1         -> return $ name1 ++ "-" ++ name2
      otherwise -> return $ name1 ++ ", " ++ name2 ++ " and " ++ name3

buzzwords :: IO String
buzzwords = randomCompanyWord "buzzwords"

bs :: IO String
bs = randomCompanyWord "bs"

suffix :: IO String
suffix = randomCompanyWord "suffix"

catchPhrase :: IO String
catchPhrase = undefined

ein :: IO String
ein = undefined

dunsNumber :: IO String
dunsNumber = undefined

logo :: IO String
logo = undefined

randomCompanyWord :: String -> IO String
randomCompanyWord attr = randomValue "company" attr
