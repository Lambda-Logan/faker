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

name :: IO String
name = do
    ind <- randomNum (0,6)
    pref <- prefix
    suff <- suffix
    fName <- firstName
    lName <- lastName
    return $ case ind of
               0         -> unwords [pref, fName, lName]
               1         -> unwords [fName, lName, suff]
               otherwise -> fName ++ " " ++ lName

firstName :: IO String
firstName = randomName "first_name"

lastName :: IO String
lastName = randomName "last_name"

prefix :: IO String
prefix = randomName "prefix"

suffix :: IO String
suffix = randomName "suffix"

title :: IO String
title = do
    descriptor <- randomTitle "descriptor"
    level      <- randomTitle "level"
    job        <- randomTitle "job"
    return $ unwords [descriptor, level, job]

randomName :: String -> IO String
randomName attr = randomValue "name" attr

randomTitle :: String -> IO String
randomTitle attr = randomValue "title" attr
