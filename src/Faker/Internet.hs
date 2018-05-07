{-|
Module        : Faker.Internet
Description   : Module for generating fake data related to Internet
Copyright     : (c) Alexey Gaziev, 2014-2018
License       : MIT
Maintainer    : alex.gaziev@gmail.com
Stability     : experimental
Portability   : POSIX

Fake data
-}
module Faker.Internet
(
-- * Functions for generate fake data related to Internet
  freeEmailDomain
, generateEmail
, domainSuffix
, email
, freeEmail
, safeEmail
, userName
)
where

import Faker.Utils
import qualified Faker.Name as N
import Data.Char
import Control.Monad (liftM)

-- | Returns random email, i.e. "hayden.pfannerstill@robel.com"
email :: Faker String
email = do
    cName  <- N.lastName
    domain <- domainSuffix
    let lDomain = loweredLetters cName ++ "." ++ domain
    generateEmail lDomain

-- | Returns random email with free domain, i.e. "lmarks@yahoo.com"
freeEmail :: Faker String
freeEmail = freeEmailDomain >>= generateEmail

-- | Returns random safe email with dummy domain, i.e. "m.langosh@example.org"
safeEmail :: Faker String
safeEmail = domainSuffix >>= generateEmail . ("example." ++)

-- | Returns random email with provided domain, i.e.
-- "a.marks@yourdomain.org"
generateEmail :: String -> Faker String
generateEmail domain = liftM (++ "@" ++ domain) userName

-- | Returns random username, i.e. "k.johnson"
userName :: Faker String
userName = do
    fName <- N.firstName
    lName <- N.lastName
    let loweredFName = loweredLetters fName
        loweredLName = loweredLetters lName
    if null loweredFName
      then userName
      else do
        ind <- randomInt (0,2)
        return $ case ind of
                0 -> loweredFName ++ "." ++ loweredLName
                1 -> head loweredFName : '.' :loweredLName
                _ -> head loweredFName : loweredLName

loweredLetters :: String -> String
loweredLetters str = map toLower $ filter isLetter str

freeEmailDomain :: Faker String
freeEmailDomain = randomInternetWord "free_email"

-- | Returns random domain suffix, i.e. "biz"
domainSuffix :: Faker String
domainSuffix = randomInternetWord "domain_suffix"

randomInternetWord :: String -> Faker String
randomInternetWord = randomValue "internet"
