module Faker.Utils
(
  valsList
, randomValue
, randomNum
, replaceSymbols
, evalRegex
)
where

import System.Random (newStdGen, randomR)
import Gimlh
import Data.List.Split (splitOn)
import Data.List (intercalate)
import Paths_faker

newtype Faker a = State FakerData a

loadFakerData :: IO SimpleGiml
loadFakerData = do
    filePath <- getDataFileName "data/en.giml"
    contents <- parseFile filePath
    return $ simplifyGiml contents

runFaker :: Faker a -> IO a
runFaker faker = do
    myData <- loadFakerData
    gen <- newStdGen
    return $ runState

valsList :: String -> IO [String]
valsList valsType = do
    filePath <- getDataFileName "data/en.giml"
    contents <- parseFile filePath
    let fetchedVal = fetch (simplifyGiml contents) valsType
    case fetchedVal of
      Nothing    -> return []
      (Just val) -> return $ val2List val

randomValue :: String -> String -> IO String
randomValue namespace valType = do
    gen <- newStdGen
    vals <- valsList (namespace ++ "$" ++ valType)
    let ind = fst $ randomR (0, length vals - 1) gen
    return $ vals !! ind

randomNum :: (Int, Int) -> IO Int
randomNum range = do
    gen <- newStdGen
    return $ fst (randomR range gen)

replaceSymbols :: String -> IO String
replaceSymbols [] = return ""
replaceSymbols (x:xs) = do
    gen <- newStdGen
    restOfLine <- replaceSymbols xs
    return $ case x of
               '#' -> show (fst (randomR (0,9) gen) :: Int) ++ restOfLine
               _   -> x : restOfLine

evalRegex :: String -> IO String
evalRegex regex = do
    let preparedRegex = if head regex == '/' && last regex == '/'
                          then init $ tail regex
                          else regex
    replaceExpressions preparedRegex >>= replaceSymbols

replaceExpressions :: String -> IO String
replaceExpressions [] = return ""
replaceExpressions [a] = return [a]
replaceExpressions (x:y:xs) = case y of
      '{' -> replicateChars x (y:xs) >>= replaceExpressions
      _   -> case x of
               '[' -> randomizeChar (x:y:xs) >>= replaceExpressions
               _   -> do
                        rest <- replaceExpressions (y:xs)
                        return $ x : rest

replicateChars :: Char -> String -> IO String
replicateChars char rest = do
  gen <- newStdGen
  let splittedLine = splitOn "}" rest
      range = read $ "(" ++ tail (head splittedLine) ++ ")" :: (Int, Int)
      replicated = replicate (fst $ randomR range gen) char
      restOfLine = intercalate "}" (tail splittedLine)
  return $ replicated ++ restOfLine

randomizeChar :: String -> IO String
randomizeChar rest = do
  gen <- newStdGen
  let splittedLine = splitOn "]" rest
      rangeNumbers = intercalate "," (splitOn "-" (tail $ head splittedLine))
      range = read $ "(" ++ rangeNumbers ++ ")" :: (Int, Int)
      randomized = show $ fst (randomR range gen)
      restOfLine = intercalate "]" (tail splittedLine)
  return $ randomized ++ restOfLine
