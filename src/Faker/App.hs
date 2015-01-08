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

name :: Faker String
name = randomAppWord "name"

version :: Faker String
version = randomAppWord "version" >>= replaceSymbols

author :: Faker String
author = do
    ind <- randomInt (0,1)
    case ind of
      0 -> N.name
      _ -> C.name

randomAppWord :: String -> Faker String
randomAppWord = randomValue "app"
