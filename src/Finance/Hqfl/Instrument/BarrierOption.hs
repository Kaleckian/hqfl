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
  BarrierOption(..)
)
where

import Finance.Hqfl.Instrument.Type

data BarrierType = DownAndIn
                 | UpAndIn
                 | DownAndOut
                 | UpAndOut
                 
data BarrierOption a = BarrierOption a BarrierType Type Style Barrier Strike Maturity
