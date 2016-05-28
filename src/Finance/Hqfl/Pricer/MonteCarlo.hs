{-# LANGUAGE FlexibleInstances #-}

module Finance.Hqfl.Pricer.MonteCarlo where

import Data.Decimal
import Finance.Hqfl.Instrument
import Control.Monad.Primitive
import Control.Monad.State
import System.Random.MWC
import System.Random.MWC.Distributions
import Data.List

-- TODO extend to handle different SDEs
-- TOOD fix types of prices: Double or Decimal?
-- TODO fix steps and dt so that they corresspond
-- TODO : Refactor to put simulation code separate from option pricing code
-- TODO : Refactor to allow different payoff functions to be passed in
-- TODO : Function to compute the average price over the path
-- TODO : Function to compute the average payoff
-- TODO : Function to compute the discounted average payoff
-- TODO : Parallelise simulation

{-

 Say the expiration is 1. Then if I want 100 steps the timestep must be (1 / 100) = 0.01.

 So the general formula is (expn / steps) = ts

-}

-- | Discretised stochastic differential equation to represent the evolution of a price
sde :: Double -> Double -> Double -> Double -> Double -> Double
--sde rate vol ts rnd price = price * exp ((rate - 0.5 * vol * vol) * ts + (vol * sqrt ts * rnd))
sde rate vol ts price rnd = price * exp ((rate - 0.5 * vol * vol) * ts + (vol * sqrt ts * rnd))

-- | Simulates the evolution of a price over a specific time horizon (uses eta reduction on sde)
walk :: PrimMonad m => Double -> Double -> Double -> Double -> Int -> Gen (PrimState m) -> m Double
walk rate vol ts price steps gen = foldl' (sde rate vol ts) price <$> replicateM steps (normal 0 1 gen)

-- | Replicates the simulation of the evolution of a price over a specific time horizon
simulate :: PrimMonad m => Int -> Int -> Double -> Double -> Double -> Double -> m [Double]
simulate paths steps price rate vol ts = create >>= \gen -> replicateM paths $ walk rate vol ts price steps gen

class MC a where
  price :: PrimMonad m => a -> m [Double]

instance MC (Option Equity) where
  price (Option (Equity p) _ _ _ _ _) = simulate 100000 100 p 0.05 0.2 0.01
