-----------------------------------------------------------------------------
-- |
-- Module      :  Finance.Hqfl.Pricer.BlackScholes
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

class BS a where
  price :: a -> Double -> Double -> Double

instance BS (Option Equity) where
  price (Option (Equity s q) m European k t) r v =
    case m of
       Call -> s * exp (-q * t) * cdf normal d1 - k * exp (-r * t) * cdf normal d2
       Put  -> k * exp (-r * t) * cdf normal (-d2) - s * exp (-q * t) * cdf normal (-d1)
    where d1 = (log (s / k) + (r - q + (v * v) / 2) * t) / (v * sqrt t)
          d2 = d1 - v * sqrt t
          normal = Normal (0 :: Double) 1

instance BS (Option StockIndex) where
  price (Option (StockIndex s q) m European k t) r v =
    case m of
       Call -> s * exp (-q * t) * cdf normal d1 - k * exp (-r * t) * cdf normal d2
       Put  -> k * exp (-r * t) * cdf normal (-d2) - s * exp (-q * t) * cdf normal (-d1)
    where d1 = (log (s / k) + (r - q + (v * v) / 2) * t) / (v * sqrt t)
          d2 = d1 - v * sqrt t
          normal = Normal (0 :: Double) 1

-- cdi :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double
-- cdi s k r v t q h = s * exp(-q * t) * (h / s)**(2 * lambda) * (cdf normal y)
--                     - k * exp(-r * t) * (h / s)**(2 * lambda - 2) * (cdf normal (y - (v * sqrt t)))
--   where lambda = ((r - q + (v * v) / 2)) / (v * v)
--         y = (log ( (h*h) / (s * k)) / v * sqrt t) + (lambda * v * sqrt t)
--         normal = Normal (0 :: Double) 1

-- cdo :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double
-- cdo s k r v t q h = s * (cdf normal x1) * exp (-q * t) - k * exp(-r * t) * (cdf normal (x1 - v * sqrt t)) -
--                     s * exp(-q * t) * (h / s)**(2 * lambda) * (cdf normal y1) + k * exp(-r * t) * (h/ s)**(2 * lambda - 2) * (cdf normal (y1 - v * sqrt t))
--   where lambda = ((r - q + (v * v) / 2)) / (v * v)
--         y1 = (log (h / s) / v * sqrt t) + (lambda * v * sqrt t)
--         x1 = (log ( s / h ) / (v * sqrt t)) + (lambda * v * sqrt t)
--         normal = Normal (0 :: Double) 1

-- cui :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double
-- cui s k r v t q h = s * (cdf normal x1) * exp(-q * t) - k * exp(-r * t) * (cdf normal (x1 - v * sqrt t)) -
--       s * exp(-q *t) * (h / s)**(2 * lambda) * ((cdf normal (-y)) - (cdf normal (-y1))) +
--       k * exp(-r * t) * (h / s)**(2 * lambda - 2) * ((cdf normal (-y + v * sqrt t)) - (cdf normal (-y1 + v * sqrt t)))
--   where lambda = ((r - q + (v * v) / 2)) / (v * v)
--         y1 = (log (h / s) / v * sqrt t) + (lambda * v * sqrt t)
--         x1 = (log ( s / h ) / (v * sqrt t)) + (lambda * v * sqrt t)
--         y = (log ( (h*h) / (s * k)) / v * sqrt t) + (lambda * v * sqrt t)
--         normal = Normal (0 :: Double) 1

-- pui :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double
-- pui s k r v t q h = -s * exp(-q * t) * (h / s)**(2 * lambda) * (cdf normal (-y)) + k * exp(-r * t) * (h / s)**(2 * lambda - 2) * (cdf normal ((-y) + (v * sqrt t)))
--   where lambda = ((r - q + (v * v) / 2)) / (v * v)
--         y = (log ( (h*h) / (s * k)) / v * sqrt t) + (lambda * v * sqrt t)
--         normal = Normal (0 :: Double) 1

-- pdi :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double
-- pdi s k r v t q h = -s * (cdf normal (-x1)) * exp(-q * t)
--                     + k * exp(-r * t) * (cdf normal (-x1 + v * sqrt t))
--                     + s * exp(-q * t) * (h / s)**(2 * lambda) * (cdf normal y - cdf normal y1)
--                     - k * exp(-r * t) * (h / s)**(2 * lambda -2) * (cdf normal (y - v * sqrt t) - cdf normal (y1 - v * sqrt t))
--   where lambda = ((r - q + (v * v) / 2)) / (v * v)
--         y1 = (log (h / s) / v * sqrt t) + (lambda * v * sqrt t)
--         x1 = (log ( s / h ) / (v * sqrt t)) + (lambda * v * sqrt t)
--         y = (log ( (h*h) / (s * k)) / v * sqrt t) + (lambda * v * sqrt t)
--         normal = Normal (0 :: Double) 1
        
-- puo s k r v t q h = -s * (cdf normal (-x1)) * exp(-q * t) + k * exp(-r * t) * (cdf normal (-x1 + v * sqrt t)) +
--                     s * exp(-q * t) * (h / s)**(2 * lambda) * (cdf normal (-y1)) - k * exp(-r * t) * (h / s)**(2 * lambda -2) * (cdf normal ((-y1) + v * sqrt t))
--   where lambda = ((r - q + (v * v) / 2)) / (v * v)
--         y1 = (log (h / s) / v * sqrt t) + (lambda * v * sqrt t)
--         x1 = (log ( s / h ) / (v * sqrt t)) + (lambda * v * sqrt t)
--         y = (log ( (h*h) / (s * k)) / v * sqrt t) + (lambda * v * sqrt t)
--         normal = Normal (0 :: Double) 1



-- instance BS (Option StockIndex) where
--   blackscholesprice (Option (StockIndex s) Call European k t) r v = call s k r v t q
--   blackscholesprice (Option (StockIndex s) Put European k t) r v = put s k r v t q
  

-- instance BS (Option Future) where
--   blackscholesprice (Option (Future f) Call European k t) r v = call f k r v t q
--   blackscholesprice (Option (Future f) Put European k t) r v = put f k r v t q

-- TODO : factor in rebates
-- instance BS (BarrierOption Equity) where
--   blackscholesprice (BarrierOption (Equity s q) x European y h k t) r v =
--     case (x, y) of
--       (DownAndIn, Call)
-- --        | s <= h -> Left "Contract is converted to a vanilla call"
--         | h <= k ->  cdi'
--         | h >= k ->   call' - cdo'
--       (DownAndOut, Call)
-- --        | s <= h -> Left "Contract is void"
--         | h <= k ->   call' - cdi'
--         | h >= k ->  cdo'
--       (UpAndIn, Call)
-- --        | h <= s -> Left "Contract is converted to a vanilla call"
--         | h <= k ->  call'
--         | h >= k ->   cui'
--       (UpAndOut, Call)
-- --        | s <= h -> Left "Contract is void"
--         | h <= k ->   call' - cdi'
--         | h >= k ->  cdo'
--       (DownAndIn, Put)
-- --        | s <= h -> Left "Contract is converted to a vanilla call"
--         | h <  k ->  pdi'
--         | h >= k ->  put'
--       (DownAndOut, Put)
-- --        | s <= h -> Left "Contract is void"
--         | h <  k ->   put' - pdi'
--         | h >=  k ->  0
--       (UpAndIn, Put)
-- --        | h <= s -> Left "Contract is converted to a vanilla call"
--         | h <= k ->   put' - puo'
--         | h >= k ->  pui'
--       (UpAndOut, Put)
-- --        | h <= s -> Left "Contract is void"
--         | h <= k ->   puo'
--         | h >= k ->   put' - pui'
--     where
--       call' = call s k r v t q 
--       cdi'  = cdi  s k r v t q h
--       cdo'  = cdo  s k r v t q h
--       cui'  = cui  s k r v t q h
--       pdi'  = pdi  s k r v t q h
--       put'  = put  s k r v t q
--       pui'  = pui  s k r v t q h
--       puo'  = puo  s k r v t q h
