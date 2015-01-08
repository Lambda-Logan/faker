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
    ind <- randomInt (0,2)
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
catchPhrase = do
    num <- randomInt (3,10)
    ws  <- sequence $ replicate num buzzwords
    return $ unwords ws

ein :: Faker String
ein = replaceSymbols "##-#######"

dunsNumber :: Faker String
dunsNumber = replaceSymbols "##-###-####"

logo :: Faker String
logo = do
    num <- randomInt (0,76)
    let lNum = num + 1
        period = if lNum < 10
                   then "00"
                   else "0"
    return $ "http://www.biz-logo.com/examples/" ++ period ++ show lNum ++ ".gif"

randomCompanyWord :: String -> Faker String
randomCompanyWord = randomValue "company"
