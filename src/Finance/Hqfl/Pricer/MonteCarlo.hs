{-# LANGUAGE FlexibleInstances #-}

module Finance.Hqfl.Pricer.MonteCarlo where

import Data.Decimal
import Finance.Hqfl.Instrument
import Control.Monad.Primitive
import Control.Monad.State
import System.Random.MWC
import System.Random.MWC.Distributions
import Data.List

-- TODO : extend to handle different SDEs
-- TODO : Refactor to put simulation code separate from option pricing code
-- TODO : Parallelise simulation

-- | Discretised stochastic differential equation to represent the evolution of a price
sde :: Double -> Double -> Double -> Double -> Double -> Double
sde rate vol ts price rnd = price * exp ((rate - 0.5 * vol * vol) * ts + (vol * sqrt ts * rnd))

-- | Simulates the evolution of a price over a specific time horizon (uses eta reduction on sde)
walk :: PrimMonad m => Double -> Double -> Double -> Double -> Int -> Gen (PrimState m) -> m Double
walk rate vol ts price steps gen = foldl' (sde rate vol ts) price <$> replicateM steps (normal 0 1 gen)

-- | Replicates the simulation of the evolution of a price over a specific time horizon
simulate :: PrimMonad m => Int -> Int -> Double -> Double -> Double -> Double -> (Double -> Double) -> m [Double]
simulate paths steps price rate vol ts pf = create >>= \gen -> replicateM paths $ pf <$> (walk rate vol ts price steps gen)

avg :: [Double] -> Double
avg xs = sum xs / fromIntegral (length xs)

discount :: Double -> Double
discount x = x * exp(-1 * 0.05)

class MC a where
  price :: PrimMonad m => a -> Int -> Int -> Double -> Double ->  m Double

-- TODO: This design might be an issue if you have american; better to create separate instances in my opinion so one instance for european puts one instance for american puts etc??
instance MC (Option Equity) where
  price (Option (Equity p) t _ s m) paths steps rate vol  = case t of
    Call -> discount <$> avg <$> simulate paths steps p rate vol (m / fromIntegral steps) (\x -> max 0 (x - s))
    Put  -> discount <$> avg <$> simulate paths steps p rate vol (m / fromIntegral steps) (\x -> max 0 (s - x))
