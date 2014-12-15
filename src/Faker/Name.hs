module Faker.Name
(
  firstName
, lastName
, fullName
, namesList
, firstNames
, name
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

firstNames :: IO [String]
firstNames = namesList "name$first_name"

lastNames :: IO [String]
lastNames = namesList "name$last_name"

name :: String -> IO String
name nameType = do
    gen <- newStdGen
    names <- case nameType of
               "first"   -> firstNames
               otherwise -> lastNames
    let ind = fst $ randomR (0, length names - 1) gen
    return $ names !! ind

firstName :: IO String
firstName = name "first"

lastName :: IO String
lastName = name "last"

fullName :: IO String
fullName = do
    first <- name "first"
    last  <- name "last"
    return $ first ++ " " ++ last
