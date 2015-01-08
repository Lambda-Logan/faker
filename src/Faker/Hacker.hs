module Faker.Hacker
(
  saySomethingSmart
, abbreviation
, adjective
, noun
, verb
, ingverb
, phrases
)
where

import Faker.Utils

saySomethingSmart :: Faker String
saySomethingSmart = do
    ind <- randomInt (0, length phrases - 1)
    evalPhrase $ phrases !! ind

abbreviation :: Faker String
abbreviation = randomHackerWord "abbreviation"

adjective :: Faker String
adjective = randomHackerWord "adjective"

noun :: Faker String
noun = randomHackerWord "noun"

verb :: Faker String
verb = randomHackerWord "verb"

ingverb :: Faker String
ingverb = randomHackerWord "ingverb"

phrases :: [String]
phrases = [ "If we #{verb} the #{noun}, we can get to the #{abbreviation} #{noun} through the #{adjective} #{abbreviation} #{noun}!"
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
