{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Control.Monad.State
import Control.Applicative
import System.Random

-- placeholder type alias for demo purposes
type GimlData = [(String, String)]

-- all the state that you need is in FakerData
data FakerData = FakerData {
    gimlData :: GimlData, -- Simplified data source for demo
    stdGen :: StdGen -- the random number generator
  }

-- wrap our state monad in our own name
newtype Faker a = Faker (State FakerData a)
  deriving (Functor, Monad, Applicative, MonadState FakerData) -- automatically make it into a monad
                                         -- we need generalized newtype deriving for this

-- placeholder for giml loading
mkGimlData :: FilePath -> IO GimlData
mkGimlData fname = return [("name", "Andrew")] -- implement something else here

-- Evaluate a faker
runFaker :: Faker a -> IO a
runFaker (Faker action) = do
  -- set up our faker data value
  gimlData <- mkGimlData "filename.ext"
  stdGen <- getStdGen
  let fakerData = FakerData { gimlData = gimlData, stdGen = stdGen }

  -- run the generator action and return the result
  return $ evalState action fakerData

-- aux function... this is just for example
readFromGiml :: String -> Faker String
readFromGiml thing = do
  -- get the giml data from the state
  d <- gets gimlData
  case  lookup thing d of -- just lookup the result as if it were a map
    Just x -> return x
    Nothing -> error "no element and sucky error handling"

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
testFaker :: Faker String
testFaker = do
  n <- readFromGiml "name"

  -- if we get lucky, get two names
  -- this is just to demo randomness
  val <- randomInt (0, 1)
  n2 <- if val == 0
         then readFromGiml "name"
         else return ""

  return $ unwords [n, n2]

main = runFaker testFaker >>= putStrLn
