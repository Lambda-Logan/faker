module Faker.Lorem
(
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

word :: Faker String
word = randomLoremWord "words"

wordOrSupplemental :: Faker String
wordOrSupplemental = do
    ind <- randomInt (0,1)
    case ind of
      0 -> word
      _ -> randomLoremWord "supplemental"

words :: Int -> Faker [String]
words num = sequence $ replicate num wordOrSupplemental

character :: Faker Char
character = do
    w <- word
    ind <- randomInt (0, length w - 1)
    return $ w !! ind

characters :: Int -> Faker [Char]
characters num = sequence $ replicate num character

sentence :: Faker String
sentence = do
    ws <- randomInt (3,12) >>= words
    let sentence = unwords ws
        result = (toUpper $ head sentence) : tail sentence ++ "."
    return result

sentences :: Int -> Faker [String]
sentences num = sequence $ replicate num sentence

paragraph :: Faker String
paragraph = randomInt (3,6) >>= sentences >>= return . unwords

paragraphs :: Int -> Faker [String]
paragraphs num = sequence $ replicate num paragraph

randomLoremWord :: String -> Faker String
randomLoremWord = randomValue "lorem"
