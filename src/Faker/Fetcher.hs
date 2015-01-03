module Faker.Fetcher
(
  valsList
, randomValue
)
where

import System.Random
import Gimlh -- need to renew

valsList :: String -> IO [String]
valsList valsType = do
    contents <- parseFile "../data/en.giml"
    let fetchedVal = fetch (simplifyGiml contents) valsType
    case fetchedVal of
      Nothing    -> return []
      (Just val) -> return $ val2List val

randomValue :: String -> String -> IO String
randomValue namespace valType = do
    gen <- newStdGen
    vals <- valsList (namespace ++ "$" ++ valType)
    let ind = fst $ randomR (0, length vals - 1) gen
    return $ vals !! ind