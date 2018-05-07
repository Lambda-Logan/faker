{-|
Module        : Faker.Hacker
Description   : Module for generating fake data related to IT
Copyright     : (c) Alexey Gaziev, 2014-2018
License       : MIT
Maintainer    : alex.gaziev@gmail.com
Stability     : experimental
Portability   : POSIX

Fake data
-}
module Faker.Hacker
  (
  -- * Functions for generate fake data related to IT
    saySomethingSmart
  , abbreviation
  , adjective
  , noun
  , verb
  , ingverb
  , phrases
  ) where

import           Faker.Utils

-- | Returns some smart IT phrase, i.e.
-- "The PCI capacitor is down, back up the digital matrix so we can hack
-- the FTP generating"
saySomethingSmart :: Faker String
saySomethingSmart = do
  ind <- randomInt (0, length phrases - 1)
  evalPhrase $ phrases !! ind

-- | Part of 'Faker.Hacker.saySomethingSmart' function.
-- Returns random IT abbreviation, i.e. "AI"
abbreviation :: Faker String
abbreviation = randomHackerWord "abbreviation"

-- | Part of 'Faker.Hacker.saySomethingSmart' function.
-- Returns random IT adjective, i.e. "primary"
adjective :: Faker String
adjective = randomHackerWord "adjective"

-- | Part of 'Faker.Hacker.saySomethingSmart' function.
-- Returns random IT noun, i.e. "panel"
noun :: Faker String
noun = randomHackerWord "noun"

-- | Part of 'Faker.Hacker.saySomethingSmart' function.
-- Returns random IT verb, i.e. "back up"
verb :: Faker String
verb = randomHackerWord "verb"

-- | Part of 'Faker.Hacker.saySomethingSmart' function.
-- Returns random IT ing verb, i.e. "copying"
ingverb :: Faker String
ingverb = randomHackerWord "ingverb"

-- | List of phrases templates for 'Faker.Hacker.saySomethingSmart'
phrases :: [String]
phrases =
  [ "If we #{verb} the #{noun}, we can get to the #{abbreviation} #{noun} through the #{adjective} #{abbreviation} #{noun}!"
  , "We need to #{verb} the #{adjective} #{abbreviation} #{noun}!"
  , "Try to #{verb} the #{abbreviation} #{noun}, maybe it will #{verb} the #{adjective} #{noun}!"
  , "You can't #{verb} the #{noun} without #{ingverb} the #{adjective} #{abbreviation} #{noun}!"
  , "Use the #{adjective} #{abbreviation} #{noun}, then you can #{verb} the #{adjective} #{noun}!"
  , "The #{abbreviation} #{noun} is down, #{verb} the #{adjective} #{noun} so we can #{verb} the #{abbreviation} #{noun}!"
  , "#{ingverb} the #{noun} won't do anything, we need to #{verb} the #{adjective} #{abbreviation} #{noun}!"
  , "I'll #{verb} the #{adjective} #{abbreviation} #{noun}, that should #{noun} the #{abbreviation} #{noun}!"
  ]

evalPhrase :: String -> Faker String
evalPhrase [] = return ""
evalPhrase phrase = do
  result <- sequence $ map evalWord (words phrase)
  return $ unwords result

evalWord :: String -> Faker String
evalWord [] = return ""
evalWord ('#':word) = do
  case init $ tail word of
    "abbreviation" -> abbreviation
    "adjective"    -> adjective
    "noun"         -> noun
    "verb"         -> verb
    _              -> ingverb
evalWord word = return word

randomHackerWord :: String -> Faker String
randomHackerWord = randomValue "hacker"
