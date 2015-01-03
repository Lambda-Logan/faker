module Faker.Name
(
  firstName
, lastName
, fullName
, prefix
, suffix
)
where

import Faker.Utils

firstName :: IO String
firstName = randomName "first_name"

lastName :: IO String
lastName = randomName "last_name"

fullName :: IO String
fullName = do
    firstPart <- firstName
    lastPart  <- lastName
    return $ firstPart ++ " " ++ lastPart

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
