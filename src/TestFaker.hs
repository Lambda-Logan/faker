{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Control.Monad.State
import Control.Applicative
import System.Random
import Gimlh

-- all the state that you need is in FakerData
data FakerData = FakerData {
    gimlData :: SimpleGiml, -- Simplified data source for demo
    stdGen :: StdGen -- the random number generator
  }

-- wrap our state monad in our own name
newtype Faker a = Faker (State FakerData a)
  deriving (Functor, Monad, Applicative, MonadState FakerData) -- automatically make it into a monad
                                         -- we need generalized newtype deriving for this

-- placeholder for giml loading
loadGimlData :: FilePath -> IO SimpleGiml
loadGimlData fname = do
    -- filePath <- getDataFileName "data/en.giml"
    contents <- parseFile fname
    return $ simplifyGiml contents

-- Evaluate a faker
runFaker :: Faker a -> IO a
runFaker (Faker action) = do
  -- set up our faker data value
  gimlData <- loadGimlData "../data/en.giml"
  stdGen <- newStdGen
  let fakerData = FakerData { gimlData = gimlData, stdGen = stdGen }

  -- run the generator action and return the result
  return $ evalState action fakerData

-- aux function... this is just for example
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
    ind <- randomInt (0, length valList)
    return $ valList !! ind

-- generate a random int and update the fakerdata state
randomInt :: (Int, Int) -> Faker Int
randomInt bounds = do
  -- get the entire state
  state <- get

  -- generate an int in the range and get the new stdgen
  let (int, newGen) = randomR bounds (stdGen state)

  -- update the state with the new stdgen
  put (state { stdGen = newGen })

  -- return the generated value
  return int

-- defining a faker
firstName :: Faker String
firstName = randomValue "name" "first_name"

main = do
  str <- runFaker $ do
    n1 <- firstName
    n2 <- firstName
    return $ n1 ++ " - " ++ n2
  putStrLn str
