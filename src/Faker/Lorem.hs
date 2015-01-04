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

word :: IO String
word = randomLoremWord "words"

randomLoremWord :: String -> IO String
randomLoremWord attr = randomValue "lorem" attr
