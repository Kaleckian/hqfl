-----------------------------------------------------------------------------
-- |
-- Module      :  Finance.Hqfl.Pricer.Asay
-- Copyright   :  (C) 2016 Mika'il Khan
-- License     :  (see the file LICENSE)
-- Maintainer  :  Mika'il Khan <co.kleisli@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
----------------------------------------------------------------------------   
{-# LANGUAGE FlexibleInstances #-}

module Finance.Hqfl.Pricer.Asay where

import Finance.Hqfl.Instrument
import Statistics.Distribution.Normal
import Data.Random

class Asay a where
  price :: a -> Double -> Double -> Double

instance Asay (Option Future) where
  price (Option (Future f) m European k t) r v =
    case m of
       Call -> f * cdf normal d1 - k * cdf normal d2
       Put  -> k * cdf normal (-d2) - f * cdf normal (-d1)
    where d1 = (log (f / k) + ((v * v) / 2) * t) / (v * sqrt t)
          d2 = d1 - v * sqrt t
          normal = Normal (0 :: Double) 1
