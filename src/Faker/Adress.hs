module Faker.Adress
(
  cityPrefix
, citySuffix
, country
, countryCode
, buildingNumber
, streetSuffix
, secondaryAdress
, postcode
, postcodeByState
, state
, stateAbbr
, timeZone
, defaultCountry
)
where

import Faker.Fetcher

cityPrefix :: IO String
cityPrefix = randomAddress "city_prefix"

citySuffix :: IO String
citySuffix = randomAddress "city_suffix"

country :: IO String
country = randomAddress "country"

countryCode :: IO String
countryCode = randomAddress "country_code"

countryCode :: IO String
countryCode = randomAddress "country_code"

buildingNumber :: IO String
buildingNumber = randomAddress "building_number"

streetSuffix :: IO String
streetSuffix = randomAddress "street_suffix"

secondaryAddress :: IO String
secondaryAddress = randomAddress "secondary_address"

postcode :: IO String
postcode = randomAddress "postcode"

postcodeByState :: IO String
postcodeByState = randomAddress "postcode_by_state"

randomAddress :: String -> IO String
randomAddress attr = randomValue "address" attr
