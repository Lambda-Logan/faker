{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-|
Module        : Faker.Utils
Description   : Module with helper functions for all other 'Faker' modules
Copyright     : (c) Alexey Gaziev, 2014-2018
License       : MIT
Maintainer    : alex.gaziev@gmail.com
Stability     : experimental
Portability   : POSIX

Fake data
-}
module Faker.Utils
  (
  -- * Data types
    Faker(..)
  , Locale(..)
  , FakerConfig(..)
  -- * Helper functions for other 'Faker' modules
  , runFaker
  , runFakerWith
  , runFakerWithSeed
  , randomValue
  , randomInt
  , replaceSymbols
  , evalRegex
  ) where

import           Control.Monad.State
import           Data.List           (intercalate)
import           Data.List.Split     (splitOn)
import           Gimlh
import           System.Random       (StdGen, mkStdGen, newStdGen, randomR)
#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif
import           Paths_faker

-- | Value represent locales
data Locale = US      -- ^ Default locale, US
            | Russian -- ^ Russian locale
  deriving (Show)

-- | Config for faker functions
newtype FakerConfig = FakerConfig
  { fakerLocale :: Locale -- ^ Contains locale for 'Faker' functions
  } deriving (Show)

-- | Fake data storage, contains default and requested locales data and
-- stdGen
data FakerData = FakerData
  { defaultLocaleData :: SimpleGiml -- ^ Fake data for default locale (for fallbacks)
  , localeData        :: SimpleGiml -- ^ Fake data for locale stored in 'FakerConfig' provided (same as 'defaultLocaleData' if none)
  , stdGen            :: StdGen     -- ^ Generator for fetching random values from data
  }

-- | Stateful type for faker values
newtype Faker a = Faker (State FakerData a)
  deriving (Functor, Monad, Applicative, MonadState FakerData)

localeFileName :: Locale -> String
localeFileName Russian = "ru"
localeFileName _       = "en"

loadGimlData :: FilePath -> IO SimpleGiml
loadGimlData fname = do
  filePath <- getDataFileName fname
  contents <- parseFile filePath
  return $ simplifyGiml contents

-- | Function for run 'Faker' functions with specific seed and default 'US'
-- locale.
runFakerWithSeed :: Int -> Faker a -> IO a
runFakerWithSeed seed (Faker action) = do
  defaultLocaleData <- getDefaultLocaleData
  let localeData = defaultLocaleData
      stdGen     = mkStdGen seed
  return $ evalState action FakerData{..}

-- | Function for run 'Faker' functions with 'FakerConfig' (currently with
-- specified locale in it)
runFakerWith :: FakerConfig -> Faker a -> IO a
runFakerWith config (Faker action) = do
  defaultLocaleData <- getDefaultLocaleData
  localeData <- getLocaleData $ fakerLocale config
  stdGen <- newStdGen
  return $ evalState action FakerData{..}

-- | Function for run 'Faker' functions with default 'US' locale
runFaker :: Faker a -> IO a
runFaker (Faker action) = do
  defaultLocaleData <- getDefaultLocaleData
  stdGen <- newStdGen
  let localeData = defaultLocaleData
  return $ evalState action FakerData{..}

getDefaultLocaleData :: IO SimpleGiml
getDefaultLocaleData = getLocaleData US

getLocaleData :: Locale -> IO SimpleGiml
getLocaleData locale =
  loadGimlData $ "data/" ++ (localeFileName locale) ++ ".giml"

readFromGiml :: String -> Faker [String]
readFromGiml thing = do
  d <- gets localeData
  defaultData <- gets defaultLocaleData
  case fetch d thing of
    Just x -> return $ val2List x
    Nothing -> case fetch defaultData thing of
      Just x  -> return $ val2List x
      Nothing -> error "no element and sucky error handling"

-- | Internal function, used in other 'Faker' modules
-- to fetch specific value from data storage by namespace and
-- value type:
--
-- >>> runFaker $ randomValue "name" "first_name"
-- "John"
randomValue :: String -> String -> Faker String
randomValue namespace valType = do
  valList <- readFromGiml (namespace ++ "$" ++ valType)
  ind <- randomInt (0, length valList - 1)
  return $ valList !! ind

-- | Internal function, used in other 'Faker' modules
-- to get random number inside provided bounds:
--
-- >>> runFaker $ randomInt (1,4)
-- 3
randomInt :: (Int, Int) -> Faker Int
randomInt bounds = do
  fakerData <- get
  let (int, newGen) = randomR bounds (stdGen fakerData)
  put (fakerData { stdGen = newGen })
  return int

-- | Internal function, used in other 'Faker' modules
-- to replace special chars '#' with random numbers
--
-- >>> runFaker $ replaceSymbols "##-##"
-- "12-48"
replaceSymbols :: String -> Faker String
replaceSymbols [] = return ""
replaceSymbols (x:xs) = do
  restOfLine <- replaceSymbols xs
  randInt <- randomInt (0,9)
  return $ case x of
    '#' -> show randInt ++ restOfLine
    _   -> x : restOfLine

-- | Internal function, used in other 'Faker' modules
-- to eval special regex and turn them into 'Faker String'
--
-- >>> runFaker $ evalRegex "/5[1-5]-#{3,5}/"
-- "555-6384"
--
-- >>> runFaker $ evalRegex "/5[1-5]-#{3,5}/"
-- "5555-177"
evalRegex :: String -> Faker String
evalRegex regex = do
  let preparedRegex =
        if head regex == '/' && last regex == '/'
          then init $ tail regex
          else regex
  replaceExpressions preparedRegex >>= replaceSymbols

replaceExpressions :: String -> Faker String
replaceExpressions [] = return ""
replaceExpressions [a] = return [a]
replaceExpressions (x:y:xs) = case y of
  '{' -> replicateChars x (y:xs) >>= replaceExpressions
  _   -> case x of
    '[' -> randomizeChar (x:y:xs) >>= replaceExpressions
    _   -> do
      rest <- replaceExpressions (y:xs)
      return $ x : rest

replicateChars :: Char -> String -> Faker String
replicateChars char rest = do
  let splittedLine = splitOn "}" rest
      range = read $ "(" ++ tail (head splittedLine) ++ ")" :: (Int, Int)
  randInt <- randomInt range
  return $ replicate randInt char ++ intercalate "}" (tail splittedLine)

randomizeChar :: String -> Faker String
randomizeChar rest = do
  let splittedLine = splitOn "]" rest
      rangeNumbers = intercalate "," (splitOn "-" (tail $ head splittedLine))
      range = read $ "(" ++ rangeNumbers ++ ")" :: (Int, Int)
  randInt <- randomInt range
  return $ show randInt ++ intercalate "]" (tail splittedLine)
