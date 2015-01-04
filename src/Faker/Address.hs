module Faker.Address
(
  city
, streetName
, streetAddress
, latitude
, longitude
, cityPrefix
, citySuffix
, country
, countryCode
, buildingNumber
, streetSuffix
, secondaryAddress
, postcode
, postcodeByState
, state
, stateAbbr
, timeZone
, defaultCountry
, randomAddress
)
where

import Faker.Utils
import qualified Faker.Name as N

city :: IO String
city = do
    fName <- N.firstName
    lName <- N.lastName
    pref  <- cityPrefix
    suff  <- citySuffix
    ind   <- randomNum (0,3)
    return $ case ind of
               0 -> pref ++ " " ++ fName ++ suff
               1 -> pref ++ " " ++ fName
               2 -> fName ++ suff
               _ -> lName ++ suff

streetName :: IO String
streetName = do
    fName <- N.firstName
    lName <- N.lastName
    suff  <- streetSuffix
    ind   <- randomNum (0,1)
    return $ case ind of
               0 -> fName ++ " " ++ suff
               _ -> lName ++ " " ++ suff

streetAddress :: IO String
streetAddress = do
    sName <- streetName
    bNum  <- buildingNumber
    return $ bNum ++ " " ++ sName

latitude :: IO String
latitude = do
    num <- randomNum (1,99)
    return . show $ ((fromIntegral num :: Double) / 100) * 180 - 90

longitude :: IO String
longitude = do
    num <- randomNum (1,99)
    return . show $ ((fromIntegral num :: Double) / 100) * 360 - 180

cityPrefix :: IO String
cityPrefix = randomAddress "city_prefix"

citySuffix :: IO String
citySuffix = randomAddress "city_suffix"

country :: IO String
country = randomAddress "country"

countryCode :: IO String
countryCode = randomAddress "country_code"

buildingNumber :: IO String
buildingNumber = randomAddress "building_number" >>= replaceSymbols

streetSuffix :: IO String
streetSuffix = randomAddress "street_suffix"

secondaryAddress :: IO String
secondaryAddress = randomAddress "secondary_address" >>= replaceSymbols

postcode :: IO String
postcode = randomAddress "postcode" >>= replaceSymbols

postcodeByState :: IO String
postcodeByState = randomAddress "postcode_by_state" >>= replaceSymbols

state :: IO String
state = randomAddress "state"

stateAbbr :: IO String
stateAbbr = randomAddress "state_abbr"

timeZone :: IO String
timeZone = randomAddress "time_zone"

defaultCountry :: IO String
defaultCountry = randomAddress "default_country"

randomAddress :: String -> IO String
randomAddress attr = randomValue "address" attr
