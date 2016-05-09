module MC where

import Data.Decimal
import Finance.Hqfl.Instrument
import Control.Monad.Primitive
import Control.Monad.State
import Data.Decimal
import System.Random.MWC
import System.Random.MWC.Distributions
import Finance.Hqfl.PricingEngine.MCVanillaOption

-- TODO extend to handle different SDEs
-- TOOD fix types of prices: Double or Decimal?
-- TODO fix steps and dt so that they corresspond
class MC a where
  price :: PrimMonad m => a -> m [Double]

instance MC Equity where
  price (Equity p) = simulate 2 2 p 0.25 0.2 0.01 
