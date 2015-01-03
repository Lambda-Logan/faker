module Faker.Name
(
  firstName
, lastName
, fullName
, prefix
, suffix
)
where

import System.Random
import Gimlh -- need to renew

namesList :: String -> IO [String]
namesList namesType = do
    contents <- parseFile "../data/en.giml"
    let fetchedVal = fetch (simplifyGiml contents) namesType
    case fetchedVal of
      Nothing    -> return []
      (Just val) -> return $ val2List val

name :: String -> IO String
name nameType = do
    gen <- newStdGen
    names <- namesList ("name$" ++ nameType)
    let ind = fst $ randomR (0, length names - 1) gen
    return $ names !! ind

firstName :: IO String
firstName = name "first_name"

lastName :: IO String
lastName = name "last_name"

fullName :: IO String
fullName = do
    firstPart <- firstName
    lastPart  <- lastName
    return $ firstPart ++ " " ++ lastPart

prefix :: IO String
prefix = name "prefix"

suffix :: IO String
suffix = name "suffix"
