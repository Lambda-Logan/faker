{-|
Module        : Faker.App
Description   : Module for generating fake data related to application
Copyright     : (c) Alexey Gaziev, 2014-2018
License       : MIT
Maintainer    : alex.gaziev@gmail.com
Stability     : experimental
Portability   : POSIX

Fake data
-}
module Faker.App
  (
  -- * Functions for generate fake data related to application
    name
  , version
  , author
  ) where

import qualified Faker.Company as C
import qualified Faker.Name    as N
import           Faker.Utils

-- | Returns random application name, i.e. "Sonair"
name :: Faker String
name = randomAppWord "name"

-- | Returns random application version, i.e. "2.6.6"
version :: Faker String
version = randomAppWord "version" >>= replaceSymbols

-- | Returns random application author's name (person or company), i.e. "Weber-Beatty"
author :: Faker String
author = do
  ind <- randomInt (0,1)
  case ind of
    0 -> N.name
    _ -> C.name

randomAppWord :: String -> Faker String
randomAppWord = randomValue "app"
