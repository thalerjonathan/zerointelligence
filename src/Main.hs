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

sellersCount :: Int
sellersCount = 500

buyersCount :: Int
buyersCount = 500

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

data Offer = Offer Price Int

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
      hClose fileHdl
--------------------------------------------------------------------------------
-- SIMULATION
--------------------------------------------------------------------------------

runSimulation :: RandomGen g => Rand g [Price]
runSimulation = do
  ss <- map Seller . take sellersCount <$> getRandomRs (0, maxCost)
  bs <- map Buyer . take buyersCount <$> getRandomRs (0, maxValue)
  
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

  return $ prices m'

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
    txTrade (Trade txp surp bIdx sIdx bv sc) = m'
      where
        m' = m {
          buyers       = removeElem bIdx (buyers m)
        , sellers      = removeElem sIdx (sellers m)
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
      (rb, bi) <- randElem (buyers m)
      newBid   <- formBidPrice rb 

      case bestAsk m of
        Nothing ->
          case bestBid m of
            Nothing -> return $ ImproveBid (Offer newBid bi)
            (Just (Offer bestBidPrice _)) ->
              if newBid > bestBidPrice
                then return $ ImproveBid (Offer newBid bi)
                else return NoTrade
        Just (Offer bestAskPrice si) -> 
          if newBid > bestAskPrice
            then do
              let (Buyer v)  = rb
                  (Seller c) = sellers m !! si
                  surp       = v - c
              return $ Match $ Trade bestAskPrice surp bi si v c
            else return NoTrade

    tradeSeller :: RandomGen g => Rand g TradeInfo
    tradeSeller = do
      (rs, si) <- randElem (sellers m)
      newAsk   <- formAskPrice rs

      case bestBid m of
        Nothing ->
          case bestAsk m of
            Nothing -> return $ ImproveAsk (Offer newAsk si)
            (Just (Offer bestAskPrice _)) ->
              if newAsk < bestAskPrice
                then return $ ImproveBid (Offer newAsk si)
                else return NoTrade
        Just (Offer bestBidPrice bi) -> 
          if bestBidPrice > newAsk
            then do
              let (Seller c) = rs
                  (Buyer v)  = buyers m !! bi 
                  surp       = v - c
              return $ Match $ Trade bestBidPrice surp bi si v c
            else return NoTrade

    formBidPrice :: RandomGen g => Buyer -> Rand g Price
    formBidPrice (Buyer v) = getRandomR (0, v)

    formAskPrice :: RandomGen g => Seller -> Rand g Price
    formAskPrice (Seller c) = getRandomR (c, maxCost)

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

removeElem :: Int -> [a] -> [a]
removeElem idx xs = take idx xs ++ drop (idx + 1) xs 

randElem :: RandomGen g => [a] -> Rand g (a, Int)
randElem xs = do
  ri <- getRandomR (0, length xs - 1)
  return (xs !! ri, ri)