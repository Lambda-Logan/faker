module Faker.Address
(
  cityPrefix
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
