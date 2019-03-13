module Main where

import Data.Maybe
import System.Random

import Control.Monad.Random

maxCost :: Double
maxCost = 25

maxValue :: Double
maxValue = 25

iterations :: Int
iterations = 50000

sellersCount :: Int
sellersCount = 500

buyersCount :: Int
buyersCount = 500

rngSeed :: Int
rngSeed = 42

-- http://people.brandeis.edu/~blebaron/classes/agentfin/GodeSunder.html

-- a seller is characterised through a uniform randomly distributed value 
-- c between 0 and maxCost
newtype Seller = Seller Double 
-- a buyer is characterised through a uniform randomly distributed value 
-- v between 0 and maxValue
newtype Buyer = Buyer Double

newtype Price = Price Double deriving Show

-- Trade contains:
-- tx price, surplus, buyer index, seller index, buyer value, seller cost
data Trade = Trade Price Price Int Int Price Price

data Market = Market
  { sellers      :: [Seller]
  , buyers       :: [Buyer]

  , bestAsk      :: Maybe (Price, Int)
  , bestBid      :: Maybe (Price, Int)

  , prices       :: [Price]
  , surplus      :: Price
  , tradedValues :: [Price]
  , tradedCosts  :: [Price]
  }

main :: IO ()
main = do
  let g  = mkStdGen 42
      ps = evalRand runSimulation g

  print ps

runSimulation :: RandomGen g => Rand g [Price]
runSimulation = do
  ss <- map Seller . take sellersCount <$> getRandomRs (0, maxCost)
  bs  <- map Buyer . take buyersCount <$> getRandomRs (0, maxValue)
  
  let m = Market {
    buyers       = bs
  , sellers      = ss
  , bestBid      = Nothing
  , bestAsk      = Nothing
  , prices       = []
  , surplus      = Price 0
  , tradedValues = []
  , tradedCosts  = []
  }

  m' <- iteration iterations m

  return $ prices m'

iteration :: RandomGen g => Int -> Market -> Rand g Market
iteration 0 m = return m -- all iterations consumed, finished
iteration n m 
  | null (buyers m) = return m -- no more agents which can trade, finished
  | otherwise = do
    -- find a trade
    mayTrade <- findTrade m
    -- no trade found, try another iteration
    if isNothing mayTrade
      then iteration (n-1) m
      else do -- found a trade, transact it
        let m' = txTrade (fromJust mayTrade) m
        iteration (n-1) m'

txTrade :: Trade -> Market -> Market
txTrade (Trade txp surp bIdx sIdx bv sc) m = m'
  where
    m' = m {
      buyers       = removeElem bIdx (buyers m)
    , sellers      = removeElem sIdx (sellers m)
    , bestBid      = bestBid m
    , bestAsk      = bestAsk m
    , prices       = txp : prices m
    , surplus      = priceSum surp (surplus m)
    , tradedValues = bv : tradedValues m
    , tradedCosts  = sc : tradedCosts m
    }

removeElem :: Int -> [a] -> [a]
removeElem idx xs = take idx xs ++ drop (idx + 1) xs 

priceSum :: Price -> Price -> Price
priceSum (Price p1) (Price p2) = Price (p1 + p2)

findTrade :: RandomGen g => Market -> Rand g (Maybe Trade)
findTrade _m = do
  tryBuyers <- getRandom
  if tryBuyers
    then do
      
      return Nothing
    else return Nothing

formBid :: Buyer -> Price
formBid (Buyer v) = Price v