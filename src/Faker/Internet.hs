module Faker.Internet
(
  freeEmail
, domainSuffix
, email
, safeEmail
, userName
)
where

import Faker.Utils
import qualified Faker.Name as N
import Data.Char

email :: IO String
email = do
    uName <- userName
    domain <- freeEmail
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

freeEmail :: IO String
freeEmail = randomInternetWord "free_email"

domainSuffix :: IO String
domainSuffix = randomInternetWord "domain_suffix"

randomInternetWord :: String -> IO String
randomInternetWord attr = randomValue "internet" attr
