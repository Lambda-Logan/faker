module Faker.Name
(
  firstNames
--, lastName
--, fullName
)
where

import System.Random
import Control.Applicative
import Control.Monad
import Data.Maybe
import Gimlh

data FirstNames = FirstNames { firstNames :: [String] }

namesList :: Giml -> String -> [String]
namesList giml, key =

firstNames' :: IO ()
firstNames' = do
    content <- BS.readFile "../data/en.yml"
    let names = Data.Yaml.decode content :: Maybe [FirstNames]
        lgth = length $ fromJust names
    print $ show lgth
    print $ fromJust names

lastNames :: [String]
lastNames = [ "Gaziev"
            , "Kapkov"
            , "Black"
            ]

-- name :: String -> IO String
-- name nameType = do
--     gen <- newStdGen
--     let names = case nameType of
--                   "first"   -> firstNames
--                   otherwise -> lastNames
--         ind = fst $ randomR (0, length names - 1) gen
--     return $ names !! ind

-- firstName :: IO String
-- firstName = name "first"

-- lastName :: IO String
-- lastName = name "last"

-- fullName :: IO String
-- fullName = do
--     first <- name "first"
--     last  <- name "last"
--     return $ first ++ " " ++ last
