module Faker.Utils
(
  valsList
, randomValue
, replaceSymbols
, evalRegex
)
where

import System.Random (newStdGen, randomR)
import Gimlh -- need to renew
import Data.List.Split (splitOn)
import Data.List (intercalate)

valsList :: String -> IO [String]
valsList valsType = do
    contents <- parseFile "../data/en.giml"
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

replaceSymbols :: String -> IO String
replaceSymbols [] = return ""
replaceSymbols (x:xs) = do
    gen <- newStdGen
    restOfLine <- replaceSymbols xs
    case x of
      '#' -> return $ (show $ (fst (randomR (0,9) gen) :: Int)) ++ restOfLine
      otherwise -> return $ x : restOfLine

evalRegex :: String -> IO String
evalRegex regex = do
    let preparedRegex = if head regex == '/' && last regex == '/'
                          then init $ tail regex
                          else regex
    replaceExpressions preparedRegex >>= replaceSymbols

replaceExpressions :: String -> IO String
replaceExpressions [] = return ""
replaceExpressions [a] = return [a]
replaceExpressions (x:y:xs) = do
    case y of
      '{'       -> replicateChars x (y:xs) >>= replaceExpressions
      otherwise -> case x of
                     '['       -> randomizeChar (x:y:xs) >>= replaceExpressions
                     otherwise -> do
                       rest <- replaceExpressions (y:xs)
                       return $ x : rest

replicateChars :: Char -> String -> IO String
replicateChars char rest = do
  gen <- newStdGen
  let splittedLine = splitOn "}" rest
      range = read $ "(" ++ (tail $ head splittedLine) ++ ")" :: (Int, Int)
      replicated = replicate (fst $ randomR range gen) char
      restOfLine = intercalate "}" (tail splittedLine)
  return $ replicated ++ restOfLine

randomizeChar :: String -> IO String
randomizeChar rest = do
  gen <- newStdGen
  let splittedLine = splitOn "]" rest
      rangeNumbers = intercalate "," (splitOn "-" (tail $ head splittedLine))
      range = read $ "(" ++ rangeNumbers ++ ")" :: (Int, Int)
      randomized = show $ (fst $ randomR range gen)
      restOfLine = intercalate "]" (tail splittedLine)
  return $ randomized ++ restOfLine
