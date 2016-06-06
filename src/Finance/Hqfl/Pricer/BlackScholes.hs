-----------------------------------------------------------------------------
-- |
-- Module      :  Finance.Hqfl.Instrument.BlackScholes
-- Copyright   :  (C) 2016 Mika'il Khan
-- License     :  (see the file LICENSE)
-- Maintainer  :  Mika'il Khan <co.kleisli@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
----------------------------------------------------------------------------   
{-# LANGUAGE FlexibleInstances #-}

module Finance.Hqfl.Pricer.BlackScholes where

import Finance.Hqfl.Instrument
import Statistics.Distribution.Normal
import Data.Random
import Data.Either

call :: Double -> Double -> Double -> Double -> Double -> Double -> Double
call s k r v t q = s * exp(-q * t) * (cdf normal d1)  - k * exp(-r * t) * (cdf normal d2)
  where d1 = (log (s / k) + (r - q + (v * v) / 2) * t) / (v * sqrt t)
        d2 = d1 - v * sqrt t
        normal = Normal (0 :: Double) 1

put :: Double -> Double -> Double -> Double -> Double -> Double
put s k r v t = k * exp(-r * t) * (cdf normal (-d2)) - s * (cdf normal (-d1))
  where d1 = (log (s / k) + (r + (v * v) / 2) * t) / (v * sqrt t)
        d2 = d1 - v * sqrt t
        normal = Normal (0 :: Double) 1

cdi :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double
cdi s k r v t q h = s * exp(-q * t) * (h / s)**(2 * lambda) * (cdf normal y)
                    - k * exp(-r * t) * (h / s)**(2 * lambda - 2) * (cdf normal (y - (v * sqrt t)))
  where lambda = ((r - q + (v * v) / 2)) / (v * v)
        y = (log ( (h*h) / (s * k)) / v * sqrt t) + (lambda * v * sqrt t)
        normal = Normal (0 :: Double) 1

cdo :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double
cdo s k r v t q h = call s k r v t q - cdi s k r v t q h


class BS a where
  blackscholesprice :: a -> Double -> Double -> Either String Double

instance BS (Option Equity) where
  blackscholesprice (Option (Equity s) Call European k t q) r v = Right $ call s k r v t q
  blackscholesprice (Option (Equity s) Put European k t q) r v = Right $ put s k r v t

{-
 What we are pricing here is the chance that given that the stock is at price s now and the barrier is h, given all the other parameters, the chance that the stock will hit the barrier or not is given by the BS equation.
-}
instance BS (BarrierOption Equity) where
  blackscholesprice (BarrierOption (Equity s) DownAndIn European Call h k t q) r v
    | s <= h = Left $ "Contract is converted to a vanilla contract"
    | h <= k = Right $ cdi s k r v t q h
  blackscholesprice (BarrierOption (Equity s) DownAndOut European Call h k t q) r v
    | s <= h = Left $ "Contract is void"
    | h <= k = Right $ cdo s k r v t q h

