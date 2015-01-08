module Faker.Name
(
  firstName
, lastName
, prefix
, suffix
, name
, title
)
where

import Faker.Utils

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

firstName :: Faker String
firstName = randomName "first_name"

lastName :: Faker String
lastName = randomName "last_name"

prefix :: Faker String
prefix = randomName "prefix"

suffix :: Faker String
suffix = randomName "suffix"

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
