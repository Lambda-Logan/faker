module Faker.Internet
(
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

email :: Faker String
email = do
    cName  <- N.lastName
    domain <- domainSuffix
    let lDomain = loweredLetters cName ++ "." ++ domain
    generateEmail lDomain

freeEmail :: Faker String
freeEmail = freeEmailDomain >>= generateEmail

safeEmail :: Faker String
safeEmail = domainSuffix >>= generateEmail . ("example." ++)

generateEmail :: String -> Faker String
generateEmail domain = liftM (++ "@" ++ domain) userName

userName :: Faker String
userName = do
    fName <- N.firstName
    lName <- N.lastName
    let loweredFName = loweredLetters fName
        loweredLName = loweredLetters lName
    ind <- randomInt (0,2)
    return $ case ind of
               0 -> loweredFName ++ "." ++ loweredLName
               1 -> head loweredFName : '.' :loweredLName
               _ -> head loweredFName : loweredLName

loweredLetters :: String -> String
loweredLetters str = map toLower $ filter isLetter str

freeEmailDomain :: Faker String
freeEmailDomain = randomInternetWord "free_email"

domainSuffix :: Faker String
domainSuffix = randomInternetWord "domain_suffix"

randomInternetWord :: String -> Faker String
randomInternetWord = randomValue "internet"
