module Faker.App
(
  name
, version
, author
)
where

import Faker.Utils
import qualified Faker.Name as N
import qualified Faker.Company as C

name :: IO String
name = randomAppWord "name"

version :: IO String
version = randomAppWord "version" >>= replaceSymbols

author :: IO String
author = do
    ind <- randomNum (0,1)
    case ind of
      0 -> N.name
      _ -> C.name

randomAppWord :: String -> IO String
randomAppWord attr = randomValue "app" attr
