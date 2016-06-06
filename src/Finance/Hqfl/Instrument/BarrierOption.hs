-----------------------------------------------------------------------------
-- |
-- Module      :  Finance.Hqfl.Instrument.BarrierOption
-- Copyright   :  (C) 2016 Mika'il Khan
-- License     :  (see the file LICENSE)
-- Maintainer  :  Mika'il Khan <co.kleisli@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
----------------------------------------------------------------------------   

module Finance.Hqfl.Instrument.BarrierOption
(
    BarrierType(..)
  , BarrierOption(..)
)
where

import Finance.Hqfl.Instrument.Type

{-

Exotic Options, Mark Rubenstein and Eric Reiener, 1992

 Barrier options are path-dependent options where the payoff depends on not only on the final price of the underlying asset but also on whether or not the underlying asset has reached some other "barrier" during the life of the option.

-}

data BarrierType = DownAndIn
                 | UpAndIn
                 | DownAndOut
                 | UpAndOut
                 
data BarrierOption a = BarrierOption a BarrierType Style Type Barrier Strike Maturity Dividend
