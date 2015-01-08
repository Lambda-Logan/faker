module Faker.Avatar
(
  image
)
where

import Faker.Utils
import qualified Faker.Lorem as L

image :: Faker String
image = do
    ws <- randomInt(2,5) >>= L.words
    let slug = foldl1 (++) ws
    return $ "http://robohash.org/" ++ slug ++ ".png?size=300x300"
