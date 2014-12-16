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
import Control.Applicative
import Control.Monad
import Data.Maybe
import Gimlh -- need to renew

fetch :: SimpleGiml -> String -> Maybe GimlVal
fetch [] _ = Nothing
fetch ((key, val):xs) req = if key == req
                              then return val
                              else fetch xs req

val2List :: GimlVal -> [String]
val2List (List val)   = val
val2List (Text val)   = [val]
val2List (Number val) = [show val]
val2List (Float val)  = [show val]

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
    first <- firstName
    last  <- lastName
    return $ first ++ " " ++ last

prefix :: IO String
prefix = name "prefix"

suffix :: IO String
suffix = name "suffix"
