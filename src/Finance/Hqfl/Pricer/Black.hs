-----------------------------------------------------------------------------
-- |
-- Module      :  Finance.Hqfl.Pricer.Black
-- Copyright   :  (C) 2016 Mika'il Khan
-- License     :  (see the file LICENSE)
-- Maintainer  :  Mika'il Khan <co.kleisli@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
----------------------------------------------------------------------------   
{-# LANGUAGE FlexibleInstances #-}

module Finance.Hqfl.Pricer.Black where

import Finance.Hqfl.Instrument
import Statistics.Distribution.Normal
import Data.Random

class Black a where
  price :: a -> Double -> Double -> Double

instance Black (Option Future) where
  price (Option (Future f) m European k t) r v =
    case m of
       Call -> exp (-r * t) * (f * cdf normal d1 - k * cdf normal d2)
       Put  -> exp (-r * t) * (k * cdf normal (-d2) - f * cdf normal (-d1))
    where d1 = (log (f / k) + ((v * v) / 2) * t) / (v * sqrt t)
          d2 = d1 - v * sqrt t
          normal = Normal (0 :: Double) 1
