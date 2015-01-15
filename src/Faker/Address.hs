{-|
Module        : Faker.Address
Description   : Module for generating fake address data
Copyright     : (c) Alexey Gaziev, 2015
License       : MIT
Maintainer    : alex.gaziev@gmail.com
Stability     : experimental
Portability   : POSIX

Fake data
-}
module Faker.Address
(
-- * Functions for generate fake address data
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

-- | Returns random city name, i.e. "West Felicitymouth"
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

-- | Returns random street name, i.e. "Alfreda Fall"
streetName :: Faker String
streetName = do
    fName <- N.firstName
    lName <- N.lastName
    suff  <- streetSuffix
    ind   <- randomInt (0,1)
    return $ case ind of
               0 -> fName ++ " " ++ suff
               _ -> lName ++ " " ++ suff

-- | Returns random street address, i.e. "78268 McDermott Run"
streetAddress :: Faker String
streetAddress = do
    sName <- streetName
    bNum  <- buildingNumber
    return $ bNum ++ " " ++ sName

-- | Returns random latitude, i.e. "21.599999999999994"
latitude :: Faker String
latitude = do
    num <- randomInt (1,99)
    return . show $ ((fromIntegral num :: Double) / 100) * 180 - 90

-- | Returns random longitude, i.e. "-97.2"
longitude :: Faker String
longitude = do
    num <- randomInt (1,99)
    return . show $ ((fromIntegral num :: Double) / 100) * 360 - 180

-- | Part of 'Faker.Address.city' function Returns random prefix for city
-- name, i.e. "Port"
cityPrefix :: Faker String
cityPrefix = randomAddress "city_prefix"

-- | Part of 'Faker.Address.city' function Returns random suffix for city
-- name, i.e. "borough"
citySuffix :: Faker String
citySuffix = randomAddress "city_suffix"

-- | Returns random country name, i.e. "Ecuador"
country :: Faker String
country = randomAddress "country"

-- | Returns random country code, i.e. "MT"
countryCode :: Faker String
countryCode = randomAddress "country_code"

-- | Part of 'Faker.Address.streetAddress' function.
-- Returns random building number, i.e. "97627"
buildingNumber :: Faker String
buildingNumber = randomAddress "building_number" >>= replaceSymbols

-- | Part of 'Faker.Address.streetName' function.
-- Returns random street suffix, i.e. "Manors"
streetSuffix :: Faker String
streetSuffix = randomAddress "street_suffix"

-- | Returns random secondary address, i.e. "Suite 242"
secondaryAddress :: Faker String
secondaryAddress = randomAddress "secondary_address" >>= replaceSymbols

-- | Returns random postcode, i.e. "87695"
postcode :: Faker String
postcode = randomAddress "postcode" >>= replaceSymbols

-- | Returns random postcode by state, i.e. "55790-3844"
postcodeByState :: Faker String
postcodeByState = randomAddress "postcode_by_state" >>= replaceSymbols

-- | Returns random US state name, i.e. "Pennsylvania"
state :: Faker String
state = randomAddress "state"

-- | Returns random US state's abbriveation, i.e. "OK"
stateAbbr :: Faker String
stateAbbr = randomAddress "state_abbr"

-- | Returns random timezone, i.e. "Africa/Harare"
timeZone :: Faker String
timeZone = randomAddress "time_zone"

-- | Returns default country for current locale, i.e. "United States of America"
defaultCountry :: Faker String
defaultCountry = randomAddress "default_country"

randomAddress :: String -> Faker String
randomAddress = randomValue "address"
