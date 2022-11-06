{-|
Module        : Faker.Lorem
Description   : Module for generating fake words, sentences and paragraphs
Copyright     : (c) Alexey Gaziev, 2014-2018
License       : MIT
Maintainer    : alex.gaziev@gmail.com
Stability     : experimental
Portability   : POSIX

Fake data
-}
module Faker.Lorem
  (
  -- * Functions for generate fake words, sentences and paragraphs
    word
  , words
  , wordsInRange
  , character
  , characters
  , charactersInRange
  , sentence
  , sentenceInRange
  , sentences
  , sentencesInRange
  , paragraph
  , paragraphInRange
  , paragraphs
  , paragraphsInRange
  ) where

import           Data.Char
import           Faker.Utils
import           Prelude     hiding (words)

-- | Returns random word, i.e. "recusandae"
word :: Faker String
word = randomLoremWord "words"

-- | Returns random word or supplemental word, i.e. "benevolentia"
wordOrSupplemental :: Faker String
wordOrSupplemental = do
  ind <- randomInt (0,1)
  case ind of
    0 -> word
    _ -> randomLoremWord "supplemental"

-- | Returns list of random words with size of provided num,
-- i.e. ["alveus","ademptio","arcus","autem","nihil"]
words :: Int -> Faker [String]
words num = sequence $ replicate num wordOrSupplemental

-- | Returns list of words within a given range
wordsInRange :: (Int, Int) -> Faker [String]
wordsInRange (x, y) = randomInt (x, y) >>= \val -> sequence $ replicate val wordOrSupplementa

-- | Returns random character, i.e. 'a'
character :: Faker Char
character = do
  w <- word
  ind <- randomInt (0, length w - 1)
  return $ w !! ind

-- | Returns random characters with size of provided num, i.e. "afasde"
characters :: Int -> Faker [Char]
characters num = sequence $ replicate num character

-- | Returns a list of characters within a given range
charactersInRange :: (Int, Int) -> Faker [Char]
charactersInRange (x, y) = randomInt (x, y) >>= (\int -> sequence $ replicate int character)

-- | Returns random sentence, i.e. "Ultio et solus uter nisi."
sentence :: Faker String
sentence = do
  ws <- randomInt (3,12) >>= words
  let ss = unwords ws
      result = (toUpper $ head ss) : tail ss ++ "."
  return result

-- | Returns random sentence where the number of words is within a given range
sentenceInRange :: (Int, Int) -> Faker String
sentenceInRange (x, y) = do
    ws <- randomInt (x, y) >>= words
    let ss = unwords ws
        result = (toUpper $ head ss) : tail ss ++ "."
    return result

-- | Returns list of random sentences with size of provided num,
-- i.e. ["Optio ago aliquid magnam bestia dolores unde.","Villa recusandae velociter assumenda."]
sentences :: Int -> Faker [String]
sentences num = sequence $ replicate num sentence

-- | Returns list of random sentences within the given range
sentencesInRange :: (Int, Int) -> Faker [String]
sentencesInRange (x, y) = randomInt(x,y) >>= \val -> sequence $ replicate val sentence

-- | Returns paragraph of random sentences (from 3 to 6),
-- i.e. "Cupressus atque civis perferendis viduo dolorem conor appositus tempore cunae. Veritatis ut tot est. Valde temperantia necessitatibus sint celo uterque sequi aduro itaque officiis quam. Statim cribro et subvenio."
paragraph :: Faker String
paragraph = randomInt (3,6) >>= sentences >>= return . unwords

-- | Returns paragraph of random sentences within a given range 
paragraphInRange :: (Int, Int) -> Faker String
paragraphInRange (x, y) = randomInt (x, y) >>= sentences >>= return . unwords

-- | Returns list of random paragraphs with size of provided num
paragraphs :: Int -> Faker [String]
paragraphs num = sequence $ replicate num paragraph

-- | Returns list of random paragraphs within a given range
paragraphsInRange :: (Int, Int) -> Faker [String]
paragraphsInRange (x, y) = randomInt(x, y) >>= \val -> sequence $ replicate val paragraph

randomLoremWord :: String -> Faker String
randomLoremWord = randomValue "lorem"
