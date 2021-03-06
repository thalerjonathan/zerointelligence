module Main where

import System.IO
import System.Random
import Text.Printf

import Control.Monad.Random

--------------------------------------------------------------------------------
-- CONSTANTS
--------------------------------------------------------------------------------

maxCost :: Double
maxCost = 25

maxValue :: Double
maxValue = 25

iterations :: Int
iterations = 50000

tradersCount :: Int
tradersCount = 500

rngSeed :: Int
rngSeed = 42

--------------------------------------------------------------------------------
-- TYPE DEFINITIONS
--------------------------------------------------------------------------------

-- http://people.brandeis.edu/~blebaron/classes/agentfin/GodeSunder.html

-- a seller is characterised through a uniform randomly distributed value 
-- c between 0 and maxCost
newtype Seller = Seller Double 
-- a buyer is characterised through a uniform randomly distributed value 
-- v between 0 and maxValue
newtype Buyer = Buyer Double

type Price = Double

-- An offer holds the price and the index of the respective agent in its list
data Offer = Offer Price Int

-- possible outcomes of trade-matching algorithm
data TradeInfo = Match Trade
               | ImproveBid Offer
               | ImproveAsk Offer
               | NoTrade

-- Trade contains:
-- tx price, surplus, buyer index, seller index, buyer value, seller cost
data Trade = Trade Price Price Int Int Price Price

data Market = Market
  { sellers      :: [Seller]
  , buyers       :: [Buyer]

  , bestAsk      :: Maybe Offer
  , bestBid      :: Maybe Offer

  , prices       :: [Price]
  , surplus      :: Price
  , tradedValues :: [Price]
  , tradedCosts  :: [Price]
  }
--------------------------------------------------------------------------------
-- MAIN
--------------------------------------------------------------------------------

main :: IO ()
main = do
    let g  = mkStdGen 42
        ps = evalRand runSimulation g

    exportPrices ps
  where
    exportPrices :: [Price] -> IO ()
    exportPrices ps = do
      fileHdl <- openFile "prices.m" WriteMode
      hPutStrLn fileHdl "prices = ["
      mapM_ (hPutStrLn fileHdl . printf "%f") ps
      hPutStrLn fileHdl "];"
      hPutStrLn fileHdl "windowSize = 50;" 
      hPutStrLn fileHdl "b = (1/windowSize)*ones(1,windowSize);"
      hPutStrLn fileHdl "a = 1;"
      hPutStrLn fileHdl "filterPrices = filter(b,a,prices);"
      hPutStrLn fileHdl "plot(filterPrices);"
      hClose fileHdl

--------------------------------------------------------------------------------
-- SIMULATION
--------------------------------------------------------------------------------

runSimulation :: RandomGen g => Rand g [Price]
runSimulation = do
  bs <- map Buyer . take tradersCount <$> getRandomRs (0, maxValue)
  ss <- map Seller . take tradersCount <$> getRandomRs (0, maxCost)

  let m = Market {
    buyers       = bs
  , sellers      = ss
  , bestBid      = Nothing
  , bestAsk      = Nothing
  , prices       = []
  , surplus      = 0
  , tradedValues = []
  , tradedCosts  = []
  }

  m' <- iteration iterations m

  -- need to reverse, because prices are appended at the front
  return $ reverse $ prices m'

iteration :: RandomGen g => Int -> Market -> Rand g Market
iteration 0 m = return m -- all iterations consumed, finished
iteration n m 
    | null (buyers m) = return m -- no more agents which can trade, finished
    | otherwise = do
      ti <- findTrade m
      case ti of
        Match t      -> iteration (n-1) (txTrade t)
        ImproveAsk o -> iteration (n-1) (improveAsk o)
        ImproveBid o -> iteration (n-1) (improveBid o)
        NoTrade      -> iteration (n-1) m

  where
    txTrade :: Trade -> Market
    txTrade (Trade txp surp bi si bv sc) = m'
      where
        m' = m {
          buyers       = removeElem bi (buyers m)
        , sellers      = removeElem si (sellers m)
        , bestAsk      = Nothing -- clear order book
        , bestBid      = Nothing -- clear order book
        , prices       = txp : prices m
        , surplus      = surp + surplus m
        , tradedValues = bv : tradedValues m
        , tradedCosts  = sc : tradedCosts m
        }

    improveAsk :: Offer -> Market
    improveAsk o = m { bestAsk = Just o }

    improveBid :: Offer -> Market
    improveBid o = m { bestBid = Just o }
    
findTrade :: RandomGen g => Market -> Rand g TradeInfo
findTrade m = do
  tryBuyers <- getRandom
  if tryBuyers
    then tradeBuyer
    else tradeSeller
  where
    tradeBuyer :: RandomGen g => Rand g TradeInfo
    tradeBuyer = do
      (rb, bi)    <- randElem (buyers m)
      newBidPrice <- formBidPrice rb 
      return $ matchBidToAsk (bestAsk m) (bestBid m) (Offer newBidPrice bi) rb

    formBidPrice :: RandomGen g => Buyer -> Rand g Price
    formBidPrice (Buyer v) = getRandomR (0, v)

    matchBidToAsk :: Maybe Offer -- ^ best ask in order book
                  -> Maybe Offer -- ^ best bid in order book
                  -> Offer       -- ^ new bid offer
                  -> Buyer       -- ^ buyer of new bid offer
                  -> TradeInfo
    -- no best ask or best bid => set this new bid offer as best bid
    matchBidToAsk Nothing Nothing o _ 
      = ImproveBid o
    -- no best ask offer but best bid => new bid might improve the best bid
    matchBidToAsk Nothing (Just (Offer bestBidPrice _)) o@(Offer newBidPrice _) _ = 
      if newBidPrice > bestBidPrice
        then ImproveBid o -- yes, it improves
        else NoTrade      -- no improvement, ignore
    -- best ask offer exists, check if cross over
    matchBidToAsk (Just (Offer bestAskPrice si)) _ (Offer newBidPrice bi) (Buyer v) =
      if newBidPrice > bestAskPrice
        then Match $ Trade bestAskPrice surp bi si v c -- yes, crossover => trade
        else NoTrade  -- no crossover, ignore
      where
        (Seller c) = sellers m !! si
        surp       = v - c

    tradeSeller :: RandomGen g => Rand g TradeInfo
    tradeSeller = do
      (rs, si)    <- randElem (sellers m)
      newAskPrice <- formAskPrice rs
      return $ matchAskToBid (bestBid m) (bestAsk m) (Offer newAskPrice si) rs

    formAskPrice :: RandomGen g => Seller -> Rand g Price
    formAskPrice (Seller c) = getRandomR (c, maxCost)

    matchAskToBid :: Maybe Offer -- ^ best bid in order book
                  -> Maybe Offer -- ^ best ask in order book
                  -> Offer       -- ^ new ask offer
                  -> Seller      -- ^ seller of new ask offer
                  -> TradeInfo
    -- no best bid or best ask => set this new ask offer as best ask
    matchAskToBid Nothing Nothing o _ 
      = ImproveAsk o
    -- no best bid offer but best ask => new ask might improve the best ask
    matchAskToBid Nothing (Just (Offer bestAskPrice _)) o@(Offer newAskPrice _) _ = 
      if newAskPrice < bestAskPrice
        then ImproveAsk o -- yes, it improves
        else NoTrade      -- no improvement, ignore
    -- best bid offer exists, check if cross over
    matchAskToBid (Just (Offer bestBidPrice bi)) _ (Offer newAskPrice si) (Seller c) =
      if bestBidPrice > newAskPrice
        then Match $ Trade bestBidPrice surp bi si v c -- yes crossover: trade!
        else NoTrade  -- no crossover, ignore
      where
        (Buyer v) = buyers m !! bi 
        surp      = v - c

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

removeElem :: Int -> [a] -> [a]
removeElem idx xs = take idx xs ++ drop (idx + 1) xs 

randElem :: RandomGen g => [a] -> Rand g (a, Int)
randElem xs = do
  ri <- getRandomR (0, length xs - 1)
  return (xs !! ri, ri)