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

put :: Double -> Double -> Double -> Double -> Double -> Double -> Double
put s k r v t q = k * exp(-r * t) * (cdf normal (-d2)) - s * exp(-q * t) * (cdf normal (-d1))
  where d1 = (log (s / k) + (r - q + (v * v) / 2) * t) / (v * sqrt t)
        d2 = d1 - v * sqrt t
        normal = Normal (0 :: Double) 1

cdi :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double
cdi s k r v t q h = s * exp(-q * t) * (h / s)**(2 * lambda) * (cdf normal y)
                    - k * exp(-r * t) * (h / s)**(2 * lambda - 2) * (cdf normal (y - (v * sqrt t)))
  where lambda = ((r - q + (v * v) / 2)) / (v * v)
        y = (log ( (h*h) / (s * k)) / v * sqrt t) + (lambda * v * sqrt t)
        normal = Normal (0 :: Double) 1

cdo :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double
cdo s k r v t q h = s * (cdf normal x1) * exp (-q * t) - k * exp(-r * t) * (cdf normal (x1 - v * sqrt t)) -
                    s * exp(-q * t) * (h / s)**(2 * lambda) * (cdf normal y1) + k * exp(-r * t) * (h/ s)**(2 * lambda - 2) * (cdf normal (y1 - v * sqrt t))
  where lambda = ((r - q + (v * v) / 2)) / (v * v)
        y1 = (log (h / s) / v * sqrt t) + (lambda * v * sqrt t)
        x1 = (log ( s / h ) / (v * sqrt t)) + (lambda * v * sqrt t)
        normal = Normal (0 :: Double) 1

cui :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double
cui s k r v t q h = s * (cdf normal x1) * exp(-q * t) - k * exp(-r * t) * (cdf normal (x1 - v * sqrt t)) -
      s * exp(-q *t) * (h / s)**(2 * lambda) * ((cdf normal (-y)) - (cdf normal (-y1))) +
      k * exp(-r * t) * (h / s)**(2 * lambda - 2) * ((cdf normal (-y + v * sqrt t)) - (cdf normal (-y1 + v * sqrt t)))
  where lambda = ((r - q + (v * v) / 2)) / (v * v)
        y1 = (log (h / s) / v * sqrt t) + (lambda * v * sqrt t)
        x1 = (log ( s / h ) / (v * sqrt t)) + (lambda * v * sqrt t)
        y = (log ( (h*h) / (s * k)) / v * sqrt t) + (lambda * v * sqrt t)
        normal = Normal (0 :: Double) 1

pui :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double
pui s k r v t q h = -s * exp(-q * t) * (h / s)**(2 * lambda) * (cdf normal (-y)) + k * exp(-r * t) * (h / s)**(2 * lambda - 2) * (cdf normal ((-y) + (v * sqrt t)))
  where lambda = ((r - q + (v * v) / 2)) / (v * v)
        y = (log ( (h*h) / (s * k)) / v * sqrt t) + (lambda * v * sqrt t)
        normal = Normal (0 :: Double) 1

pdi :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double
pdi s k r v t q h = -s * (cdf normal (-x1)) * exp(-q * t)
                    + k * exp(-r * t) * (cdf normal (-x1 + v * sqrt t))
                    + s * exp(-q * t) * (h / s)**(2 * lambda) * (cdf normal y - cdf normal y1)
                    - k * exp(-r * t) * (h / s)**(2 * lambda -2) * (cdf normal (y - v * sqrt t) - cdf normal (y1 - v * sqrt t))
  where lambda = ((r - q + (v * v) / 2)) / (v * v)
        y1 = (log (h / s) / v * sqrt t) + (lambda * v * sqrt t)
        x1 = (log ( s / h ) / (v * sqrt t)) + (lambda * v * sqrt t)
        y = (log ( (h*h) / (s * k)) / v * sqrt t) + (lambda * v * sqrt t)
        normal = Normal (0 :: Double) 1
        
puo s k r v t q h = -s * (cdf normal (-x1)) * exp(-q * t) + k * exp(-r * t) * (cdf normal (-x1 + v * sqrt t)) +
                    s * exp(-q * t) * (h / s)**(2 * lambda) * (cdf normal (-y1)) - k * exp(-r * t) * (h / s)**(2 * lambda -2) * (cdf normal ((-y1) + v * sqrt t))
  where lambda = ((r - q + (v * v) / 2)) / (v * v)
        y1 = (log (h / s) / v * sqrt t) + (lambda * v * sqrt t)
        x1 = (log ( s / h ) / (v * sqrt t)) + (lambda * v * sqrt t)
        y = (log ( (h*h) / (s * k)) / v * sqrt t) + (lambda * v * sqrt t)
        normal = Normal (0 :: Double) 1
    
class BS a where
  blackscholesprice :: a -> Double -> Double -> Either String Double

instance BS (Option Equity) where
  blackscholesprice (Option (Equity s) Call European k t q) r v = Right $ call s k r v t q
  blackscholesprice (Option (Equity s) Put European k t q) r v = Right $ put s k r v t q

-- TODO : factor in rebates
instance BS (BarrierOption Equity) where
  blackscholesprice (BarrierOption (Equity s) x European y h k t q) r v =
    case (x, y) of
      (DownAndIn, Call)
        | s <= h -> Left "Contract is converted to a vanilla call"
        | h <= k -> Right cdi'
        | h >= k -> Right $ call' - cdo'
      (DownAndOut, Call)
        | s <= h -> Left "Contract is void"
        | h <= k -> Right $ call' - cdi'
        | h >= k -> Right cdo'
      (UpAndIn, Call)
        | h <= s -> Left "Contract is converted to a vanilla call"
        | h <= k -> Right call'
        | h >= k -> Right $ cui'
      (UpAndOut, Call)
        | s <= h -> Left "Contract is void"
        | h <= k -> Right $ call' - cdi'
        | h >= k -> Right cdo'
      (DownAndIn, Put)
        | s <= h -> Left "Contract is converted to a vanilla call"
        | h <  k -> Right pdi'
        | h >= k -> Right put'
      (DownAndOut, Put)
        | s <= h -> Left "Contract is void"
        | h <  k -> Right $ put' - pdi'
        | h >=  k -> Right 0
      (UpAndIn, Put)
        | h <= s -> Left "Contract is converted to a vanilla call"
        | h <= k -> Right $ put' - puo'
        | h >= k -> Right pui'
      (UpAndOut, Put)
        | h <= s -> Left "Contract is void"
        | h <= k -> Right $ puo'
        | h >= k -> Right $ put' - pui'
    where
      call' = call s k r v t q 
      cdi'  = cdi  s k r v t q h
      cdo'  = cdo  s k r v t q h
      cui'  = cui  s k r v t q h
      pdi'  = pdi  s k r v t q h
      put'  = put  s k r v t q
      pui'  = pui  s k r v t q h
      puo'  = puo  s k r v t q h
