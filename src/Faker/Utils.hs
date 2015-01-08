{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Faker.Utils
(
-- * Data types
  Faker(..)

-- * functions
, runFaker
, randomValue
, randomInt
, replaceSymbols
, evalRegex
)
where

import System.Random (newStdGen, StdGen, randomR)
import Gimlh
import Data.List.Split (splitOn)
import Data.List (intercalate)
import Control.Monad.State
import Control.Applicative
--import Paths_faker

data FakerData = FakerData {
    gimlData :: SimpleGiml,
    stdGen :: StdGen
  }

newtype Faker a = Faker (State FakerData a)
  deriving (Functor, Monad, Applicative, MonadState FakerData)

loadGimlData :: FilePath -> IO SimpleGiml
loadGimlData fname = do
    -- filePath <- getDataFileName "data/en.giml"
    contents <- parseFile fname
    return $ simplifyGiml contents

runFaker :: Faker a -> IO a
runFaker (Faker action) = do
  gimlData <- loadGimlData "../data/en.giml"
  stdGen <- newStdGen
  let fakerData = FakerData { gimlData = gimlData, stdGen = stdGen }

  return $ evalState action fakerData

readFromGiml :: String -> Faker [String]
readFromGiml thing = do
  -- get the giml data from the state
  d <- gets gimlData
  case fetch d thing of -- just lookup the result as if it were a map
    Just x -> return $ val2List x
    Nothing -> error "no element and sucky error handling"

randomValue :: String -> String -> Faker String
randomValue namespace valType = do
    valList <- readFromGiml (namespace ++ "$" ++ valType)
    ind <- randomInt (0, length valList - 1)
    return $ valList !! ind

randomInt :: (Int, Int) -> Faker Int
randomInt bounds = do
  state <- get

  let (int, newGen) = randomR bounds (stdGen state)

  put (state { stdGen = newGen })

  return int

replaceSymbols :: String -> Faker String
replaceSymbols [] = return ""
replaceSymbols (x:xs) = do
    restOfLine <- replaceSymbols xs
    randInt <- randomInt (0,9)
    return $ case x of
               '#' -> show randInt ++ restOfLine
               _   -> x : restOfLine

evalRegex :: String -> Faker String
evalRegex regex = do
    let preparedRegex = if head regex == '/' && last regex == '/'
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
