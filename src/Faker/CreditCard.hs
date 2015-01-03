module Faker.CreditCard
(
  visa
, mastercard
, discover
, americanExpress
, dinersClub
, jcb
, switch
, solo
, dankort
, maestro
, forbrugsforeningen
, laser
)
where

import Data.Char (digitToInt, isDigit)
import Faker.Utils

visa :: IO String
visa = randomCardNumber "visa"

mastercard :: IO String
mastercard = randomCardNumber "mastercard"

discover :: IO String
discover = randomCardNumber "discover"

americanExpress :: IO String
americanExpress = randomCardNumber "american_express"

dinersClub :: IO String
dinersClub = randomCardNumber "dinersClub"

jcb :: IO String
jcb = randomCardNumber "jcb"

switch :: IO String
switch = randomCardNumber "switch"

solo :: IO String
solo = randomCardNumber "solo"

dankort :: IO String
dankort = randomCardNumber "dankort"

maestro :: IO String
maestro = randomCardNumber "maestro"

forbrugsforeningen :: IO String
forbrugsforeningen = randomCardNumber "forbrugsforeningen"

laser :: IO String
laser = randomCardNumber "laser"

randomCardNumber :: String -> IO String
randomCardNumber attr = randomValue "credit_card" attr >>= evalRegex >>= addLuhnSum

addLuhnSum :: String -> IO String
addLuhnSum numberString = do
    let numbers = collectNumbers numberString
        luhnSum = countLuhnSum numbers 2
        luhnDigit = (10 - (luhnSum `mod` 10)) `mod` 10
    return $ (init numberString) ++ (show luhnDigit)

countLuhnSum :: [Int] -> Int -> Int
countLuhnSum [] _ = 0
countLuhnSum (x:xs) m = let nextM = if m == 2 then 1 else 2 in
                            (luhnStep x m) + (countLuhnSum xs nextM)

luhnStep :: Int -> Int -> Int
luhnStep x m = sum $ map digitToInt (show (x * m))

collectNumbers :: String -> [Int]
collectNumbers [] = []
collectNumbers str = foldl (\a x -> if isDigit x then (digitToInt x) : a else a) [] str

