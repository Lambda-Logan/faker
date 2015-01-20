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

-- | Returns random diners card number, i.e. "3058-931015-6480"
dinersClub :: Faker String
dinersClub = randomCardNumber "diners_club"

-- | Returns random jsb card number, i.e. "3529-3170-1533-8944"
jcb :: Faker String
jcb = randomCardNumber "jcb"

-- | Returns random switch card number, i.e. "6759-8669-0174-5662-863"
switch :: Faker String
switch = randomCardNumber "switch"

-- | Returns random solo card number, i.e. "6767-9171-7219-8374-98"
solo :: Faker String
solo = randomCardNumber "solo"

-- | Returns random dankort card number, i.e. "5019-5391-9757-3574"
dankort :: Faker String
dankort = randomCardNumber "dankort"

-- | Returns random maestro card number, i.e. "563427125821696744"
maestro :: Faker String
maestro = randomCardNumber "maestro"

-- | Returns random forbrugsforeningen card number, i.e. "6007-2299-2494-9683"
forbrugsforeningen :: Faker String
forbrugsforeningen = randomCardNumber "forbrugsforeningen"

-- | Returns random laser card number, i.e. "6709272591057118"
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

