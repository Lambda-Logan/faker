{-|
Module        : Faker.Lorem
Description   : Module for generating fake words, sentences and paragraphs
Copyright     : (c) Alexey Gaziev, 2015
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
, character
, characters
, sentence
, sentences
, paragraph
, paragraphs
)
where

import Faker.Utils
import Prelude hiding (words)
import Data.Char

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

-- | Returns random character, i.e. 'a'
character :: Faker Char
character = do
    w <- word
    ind <- randomInt (0, length w - 1)
    return $ w !! ind

-- | Returns random characters with size of provided num, i.e. "afasde"
characters :: Int -> Faker [Char]
characters num = sequence $ replicate num character

-- | Returns random sentence, i.e. "Ultio et solus uter nisi."
sentence :: Faker String
sentence = do
    ws <- randomInt (3,12) >>= words
    let ss = unwords ws
        result = (toUpper $ head ss) : tail ss ++ "."
    return result

-- | Returns list of random sentences with size of provided num,
-- i.e. ["Optio ago aliquid magnam bestia dolores unde.","Villa recusandae velociter assumenda."]
sentences :: Int -> Faker [String]
sentences num = sequence $ replicate num sentence

-- | Returns paragraph, of random sentences (from 3 to 6),
-- i.e. "Cupressus atque civis perferendis viduo dolorem conor appositus tempore cunae. Veritatis ut tot est. Valde temperantia necessitatibus sint celo uterque sequi aduro itaque officiis quam. Statim cribro et subvenio."
paragraph :: Faker String
paragraph = randomInt (3,6) >>= sentences >>= return . unwords

-- | Returns list of random paragraphs with size of provided num
paragraphs :: Int -> Faker [String]
paragraphs num = sequence $ replicate num paragraph

randomLoremWord :: String -> Faker String
randomLoremWord = randomValue "lorem"
