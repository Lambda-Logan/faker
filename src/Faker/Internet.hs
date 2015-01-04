module Faker.Internet
(
  freeEmailDomain
, domainSuffix
, email
, freeEmail
, safeEmail
, userName
)
where

import Faker.Utils
import qualified Faker.Name as N
import qualified Faker.Company as C
import Data.Char

email :: IO String
email = do
    uName <- userName
    domain <- domainSuffix
    cName <- N.lastName
    let lDomain = (loweredLetters cName) ++ "." ++ domain
    return $ uName ++ "@" ++ lDomain

freeEmail :: IO String
freeEmail = do
    uName <- userName
    domain <- freeEmailDomain
    return $ uName ++ "@" ++ domain

safeEmail :: IO String
safeEmail = do
    uName <- userName
    domain <- domainSuffix
    return $ uName ++ "@example." ++ domain

userName :: IO String
userName = do
    fName <- N.firstName
    lName <- N.lastName
    let loweredFName = loweredLetters fName
        loweredLName = loweredLetters lName
    ind <- randomNum (0,2)
    case ind of
      0 -> return $ loweredFName ++ "." ++ loweredLName
      1 -> return $ (head loweredFName):'.':loweredLName
      2 -> return $ (head loweredFName):loweredLName

loweredLetters :: String -> String
loweredLetters str = map toLower $ filter isLetter str

freeEmailDomain :: IO String
freeEmailDomain = randomInternetWord "free_email"

domainSuffix :: IO String
domainSuffix = randomInternetWord "domain_suffix"

randomInternetWord :: String -> IO String
randomInternetWord attr = randomValue "internet" attr
