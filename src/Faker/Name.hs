{-|
Module        : Faker.Name
Description   : Module for generating fake names and titles
Copyright     : (c) Alexey Gaziev, 2014-2018
License       : MIT
Maintainer    : alex.gaziev@gmail.com
Stability     : experimental
Portability   : POSIX

Fake data
-}
module Faker.Name
(
-- * Functions for generate fake names and titles
  firstName
, lastName
, prefix
, suffix
, name
, title
)
where

import Faker.Utils

-- | Returns random full name, sometimes with preffix or suffix, i.e.
-- "Cara Gulgowski III"
name :: Faker String
name = do
    ind <- randomInt (0,6)
    pref <- prefix
    suff <- suffix
    fName <- firstName
    lName <- lastName
    return $ case ind of
               0 -> unwords [pref, fName, lName]
               1 -> unwords [fName, lName, suff]
               _ -> fName ++ " " ++ lName

-- | Returns random first name, i.e. "Juston"
firstName :: Faker String
firstName = randomName "first_name"

-- | Returns random last name, i.e. "Pfannerstill"
lastName :: Faker String
lastName = randomName "last_name"

-- | Returns random prefix, i.e. "Miss"
prefix :: Faker String
prefix = randomName "prefix"

-- | Returns random suffix, i.e. "Jr."
suffix :: Faker String
suffix = randomName "suffix"

-- | Returns random title, i.e. "Principal Configuration Assistant"
title :: Faker String
title = do
    descriptor <- randomTitle "descriptor"
    level      <- randomTitle "level"
    job        <- randomTitle "job"
    return $ unwords [descriptor, level, job]

randomName :: String -> Faker String
randomName = randomValue "name"

randomTitle :: String -> Faker String
randomTitle = randomValue "title"
