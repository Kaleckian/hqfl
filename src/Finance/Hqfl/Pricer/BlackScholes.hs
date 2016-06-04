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

call :: Double -> Double -> Double -> Double -> Double -> Double
call s k r v t = s * (cdf normal d1) - k * exp(-r * t) * (cdf normal d2)
  where d1 = (log (s / k) + (r + (v * v) / 2) * t) / (v * sqrt t)
        d2 = d1 - v * sqrt t
        normal = Normal (0 :: Double) 1

put :: Double -> Double -> Double -> Double -> Double -> Double
put s k r v t = k * exp(-r * t) * (cdf normal (-d2)) - s * (cdf normal (-d1))
  where d1 = (log (s / k) + (r + (v * v) / 2) * t) / (v * sqrt t)
        d2 = d1 - v * sqrt t
        normal = Normal (0 :: Double) 1

class BS a where
  blackscholesprice :: a -> Double -> Double -> Double

instance BS (Option Equity) where
  blackscholesprice (Option (Equity s) Call European k t) r v = call s k r v t
  blackscholesprice (Option (Equity s) Put European k t) r v = put s k r v t
