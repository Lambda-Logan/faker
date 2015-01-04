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

word :: IO String
word = randomLoremWord "words"

words :: IO String
words = undefined

character :: IO String
character = undefined

characters :: IO String
characters = undefined

sentence :: IO String
sentence = undefined

sentences :: IO String
sentences = undefined

paragraph :: IO String
paragraph = undefined

paragraphs :: IO String
paragraphs = undefined

randomLoremWord :: String -> IO String
randomLoremWord attr = randomValue "lorem" attr
