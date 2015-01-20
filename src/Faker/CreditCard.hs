{-|
Module        : Faker.App
Description   : Module for generating fake credit card numbers
Copyright     : (c) Alexey Gaziev, 2015
License       : MIT
Maintainer    : alex.gaziev@gmail.com
Stability     : experimental
Portability   : POSIX

Fake data
-}
module Faker.CreditCard
(
-- * Functions for generate fake credit card numbers
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

-- | Returns random visa card number, i.e. "4784066907150"
visa :: Faker String
visa = randomCardNumber "visa"

-- | Returns random mastercard card number, i.e. "5524-7275-2305-9123"
mastercard :: Faker String
mastercard = randomCardNumber "mastercard"

-- | Returns random discover card number, i.e. "6485-6297-9249-9908-4511"
discover :: Faker String
discover = randomCardNumber "discover"

-- | Returns random discover card number, i.e. "3772-746109-17862"
americanExpress :: Faker String
americanExpress = randomCardNumber "american_express"

-- | Returns random discover card number, i.e. "3772-746109-17862"
dinersClub :: Faker String
dinersClub = randomCardNumber "diners_club"

jcb :: Faker String
jcb = randomCardNumber "jcb"

switch :: Faker String
switch = randomCardNumber "switch"

solo :: Faker String
solo = randomCardNumber "solo"

dankort :: Faker String
dankort = randomCardNumber "dankort"

maestro :: Faker String
maestro = randomCardNumber "maestro"

forbrugsforeningen :: Faker String
forbrugsforeningen = randomCardNumber "forbrugsforeningen"

laser :: Faker String
laser = randomCardNumber "laser"

randomCardNumber :: String -> Faker String
randomCardNumber attr = do
    cardNum <- randomValue "credit_card" attr
    filledNum <- evalRegex cardNum
    return $ addLuhnSum filledNum

addLuhnSum :: String -> String
addLuhnSum numberString = let
    numbers = collectNumbers numberString
    luhnSum = countLuhnSum numbers 2
    luhnDigit = (10 - (luhnSum `mod` 10)) `mod` 10
  in
    init numberString ++ show luhnDigit

countLuhnSum :: [Int] -> Int -> Int
countLuhnSum [] _ = 0
countLuhnSum (x:xs) m = let nextM = if m == 2 then 1 else 2 in
                            luhnStep x m + countLuhnSum xs nextM

luhnStep :: Int -> Int -> Int
luhnStep x m = sum $ map digitToInt (show (x * m))

collectNumbers :: String -> [Int]
collectNumbers [] = []
collectNumbers str = foldl (\a x -> if isDigit x then digitToInt x : a else a) [] str

