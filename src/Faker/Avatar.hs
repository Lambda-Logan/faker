{-|
Module        : Faker.Avatar
Description   : Module for generating fake avatar url
Copyright     : (c) Alexey Gaziev, 2015
License       : MIT
Maintainer    : alex.gaziev@gmail.com
Stability     : experimental
Portability   : POSIX

Fake data
-}
module Faker.Avatar
(
-- * Function for generate fake avatar url
  image
)
where

import Faker.Utils
import qualified Faker.Lorem as L

-- | Returns random avatar url from robohash.org with 300x300 size,
-- i.e. "http://robohash.org/suntbestia.png?size=300x300"
image :: Faker String
image = do
    ws <- randomInt(2,5) >>= L.words
    let slug = foldl1 (++) ws
    return $ "http://robohash.org/" ++ slug ++ ".png?size=300x300"
