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

email :: IO String
email = do
    cName  <- N.lastName
    domain <- domainSuffix
    let lDomain = (loweredLetters cName) ++ "." ++ domain
    generateEmail lDomain

freeEmail :: IO String
freeEmail = freeEmailDomain >>= generateEmail

safeEmail :: IO String
safeEmail = domainSuffix >>= generateEmail . ("example." ++)

generateEmail :: String -> IO String
generateEmail domain = userName >>= return . (++ "@" ++ domain)

userName :: IO String
userName = do
    fName <- N.firstName
    lName <- N.lastName
    let loweredFName = loweredLetters fName
        loweredLName = loweredLetters lName
    ind <- randomNum (0,2)
    return $ case ind of
               0 -> loweredFName ++ "." ++ loweredLName
               1 -> (head loweredFName):'.':loweredLName
               _ -> (head loweredFName):loweredLName

loweredLetters :: String -> String
loweredLetters str = map toLower $ filter isLetter str

freeEmailDomain :: IO String
freeEmailDomain = randomInternetWord "free_email"

domainSuffix :: IO String
domainSuffix = randomInternetWord "domain_suffix"

randomInternetWord :: String -> IO String
randomInternetWord attr = randomValue "internet" attr
