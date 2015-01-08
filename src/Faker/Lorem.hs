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

word :: Faker String
word = randomLoremWord "words"

words :: Faker String
words = undefined

character :: Faker String
character = undefined

characters :: Faker String
characters = undefined

sentence :: Faker String
sentence = undefined

sentences :: Faker String
sentences = undefined

paragraph :: Faker String
paragraph = undefined

paragraphs :: Faker String
paragraphs = undefined

randomLoremWord :: String -> Faker String
randomLoremWord = randomValue "lorem"
