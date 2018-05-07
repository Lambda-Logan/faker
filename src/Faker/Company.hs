{-|
Module        : Faker.Company
Description   : Module for generating fake data related to company
Copyright     : (c) Alexey Gaziev, 2014-2018
License       : MIT
Maintainer    : alex.gaziev@gmail.com
Stability     : experimental
Portability   : POSIX

Fake data
-}
module Faker.Company
(
-- * Functions for generate fake data related to company
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

-- | Returns random company name, i.e. "Sonair"
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

-- | Returns random buzzword related to companies, i.e. "Balanced"
buzzwords :: Faker String
buzzwords = randomCompanyWord "buzzwords"

-- | Returns random bullshit word related to companies, i.e. "deliver"
bs :: Faker String
bs = randomCompanyWord "bs"

-- | Returns random suffix related to companies, i.e. "Inc"
suffix :: Faker String
suffix = randomCompanyWord "suffix"

-- | Returns random catch phrase from buzzwords related to companies,
-- i.e. "Switchable Realigned full-range zero tolerance database Monitored
-- data-warehouse monitoring installation"
catchPhrase :: Faker String
catchPhrase = do
    num <- randomInt (3,10)
    ws  <- sequence $ replicate num buzzwords
    return $ unwords ws

-- | Returns random company ein, i.e. "12-1234567"
ein :: Faker String
ein = replaceSymbols "##-#######"

-- | Returns random company duns number, i.e. "12-123-4567"
dunsNumber :: Faker String
dunsNumber = replaceSymbols "##-###-####"

-- | Returns random company logo url from biz-logo site,
-- i.e. "http://www.biz-logo.com/examples/057.gif"
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
