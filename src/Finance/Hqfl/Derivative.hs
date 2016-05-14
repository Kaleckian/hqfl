{-# LANGUAGE FlexibleInstances #-}
module Finance.Hqfl.Derivative
       (
       ) where

import Finance.Hqfl.Instrument.Equity
import Finance.Hqfl.Instrument.Option

{-
Now I can use this class to constrain the types the simulator
or other functions can accept as types
-}
class Derivative a where
  price3 :: a

instance Derivative (Option Equity) where
  price3 = (Option (Equity 100) Call European 100 2.0 2)

  -- this will be a call to a function here that will make a calll to simulate and then apply the payooff func



