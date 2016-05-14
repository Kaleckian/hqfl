{-# LANGUAGE FlexibleInstances #-}

module MonteCarlo where

import Data.Decimal
import Finance.Hqfl.Instrument
import Control.Monad.Primitive
import Control.Monad.State
import System.Random.MWC
import System.Random.MWC.Distributions

-- TODO extend to handle different SDEs
-- TOOD fix types of prices: Double or Decimal?
-- TODO fix steps and dt so that they corresspond
-- TODO : Refactor to put simulation code separate from option pricing code
-- TODO : Refactor to allow different payoff functions to be passed in
-- TODO : Function to compute the average price over the path
-- TODO : Function to compute the average payoff
-- TODO : Function to compute the discounted average payoff
-- TODO : Parallelise simulation

-- | Discretised stochastic differential equation to represent the evolution of a price
sde :: Double -> Double -> Double -> Double -> Double -> Double
sde rate vol ts rnd price = price * exp ((rate - 0.5 * vol * vol) * ts + (vol * sqrt ts * rnd))

-- | Simulates the evolution of a price over a specific time horizon (uses eta reduction on sde)
walk :: PrimMonad m => Double -> Double -> Double -> Double -> Int -> Gen (PrimState m) -> m Double
walk rate vol ts price steps gen = foldr (sde rate vol ts) price <$> replicateM steps (normal 0 1 gen)

-- | Replicates the simulation of the evolution of a price over a specific time horizon
simulate :: PrimMonad m => Int -> Int -> Double -> Double -> Double -> Double -> m [Double]
simulate paths steps price rate vol ts = create >>= \gen -> replicateM paths $ walk rate vol ts price steps gen

class MC a where
  price :: PrimMonad m => a -> m [Double]

instance MC (Option Equity) where
  price (Option (Equity p) _ _ _ _ _ ) = simulate 2 2 p 0.25 0.2 0.01
