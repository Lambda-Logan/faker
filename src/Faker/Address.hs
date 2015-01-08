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

city :: Faker String
city = do
    fName <- N.firstName
    lName <- N.lastName
    pref  <- cityPrefix
    suff  <- citySuffix
    ind   <- randomInt (0,3)
    return $ case ind of
               0 -> pref ++ " " ++ fName ++ suff
               1 -> pref ++ " " ++ fName
               2 -> fName ++ suff
               _ -> lName ++ suff

streetName :: Faker String
streetName = do
    fName <- N.firstName
    lName <- N.lastName
    suff  <- streetSuffix
    ind   <- randomInt (0,1)
    return $ case ind of
               0 -> fName ++ " " ++ suff
               _ -> lName ++ " " ++ suff

streetAddress :: Faker String
streetAddress = do
    sName <- streetName
    bNum  <- buildingNumber
    return $ bNum ++ " " ++ sName

latitude :: Faker String
latitude = do
    num <- randomInt (1,99)
    return . show $ ((fromIntegral num :: Double) / 100) * 180 - 90

longitude :: Faker String
longitude = do
    num <- randomInt (1,99)
    return . show $ ((fromIntegral num :: Double) / 100) * 360 - 180

cityPrefix :: Faker String
cityPrefix = randomAddress "city_prefix"

citySuffix :: Faker String
citySuffix = randomAddress "city_suffix"

country :: Faker String
country = randomAddress "country"

countryCode :: Faker String
countryCode = randomAddress "country_code"

buildingNumber :: Faker String
buildingNumber = randomAddress "building_number" >>= replaceSymbols

streetSuffix :: Faker String
streetSuffix = randomAddress "street_suffix"

secondaryAddress :: Faker String
secondaryAddress = randomAddress "secondary_address" >>= replaceSymbols

postcode :: Faker String
postcode = randomAddress "postcode" >>= replaceSymbols

postcodeByState :: Faker String
postcodeByState = randomAddress "postcode_by_state" >>= replaceSymbols

state :: Faker String
state = randomAddress "state"

stateAbbr :: Faker String
stateAbbr = randomAddress "state_abbr"

timeZone :: Faker String
timeZone = randomAddress "time_zone"

defaultCountry :: Faker String
defaultCountry = randomAddress "default_country"

randomAddress :: String -> Faker String
randomAddress = randomValue "address"
