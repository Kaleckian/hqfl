-----------------------------------------------------------------------------
-- |
-- Module      :  Finance.Hqfl.Instrument.Swaps
-- Copyright   :  (C) 2018 Mika'il Khan
-- License     :  (see the file LICENSE)
-- Maintainer  :  Mika'il Khan <co.category@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
----------------------------------------------------------------------------   

module Finance.Hqfl.Instrument.Swaps
  (
    Swaps
  )
  where

import qualified Data.Map as Map

-- https://en.wikipedia.org/wiki/Swap_(finance)
-- https://www.investopedia.com/articles/trading/11/introduction-swap-market.asp

data Swaps = InterestRateSwaps
           | CurrencySwaps
           | AssetSwap
           | BasisSwap
           | InflationSwap

type Maturity = String
type SwapCurve = Map.Map Maturity SwapRate
type LiborCurve = Map.Map Maturity LiborRate
type LiborRate = Double
type SwapRate = Double
type Principal = Double
type Frequency = Integer

netCashFlow :: Swaps -> LiborRate -> SwapRate -> Principal  -> Double
netCashFlow s l r p f = floatingRatePayment l p f - fixedRatePayment r p f


{-
 https://www.investopedia.com/terms/a/absoluterate.asp
make this function
--}
  
--absoluteRate :: Swap -> Double

--netCash :: Swap -> SwapCurve -> FixedLeg -> FloatingLeg -> Double

{-
A is a fixed rate payer
B is a floating rate payer

The floating rate payer has Libor rate risk.

THe payments are netted so that the direction of the payment is determined by the sign on the amout
after netting

The floating rate is usually the libor rate

 Say the 6 month libor rate is 4.2% per annum then for a 6 month period I need apply only 2.1% which gets applied to calculation
So, I need to have a mapping to say 6month -> 0.5
What would it be for the overnight rate?

Take the rate at the beginning of the contract
After the first period is over, apply that rate to work out the floating rate payment
Calculate fixed rate payment
Net Cash Flow
Work out direction of payment

So this routine will always be called when it's schedule dictates

The variable is the Libor rate`x


-}


{-
 I need lookup the libor rate for a given date
 For the purposes of testing, we can have a simple map structure which mimics a database or a file
 Anybody who wants to use a database needs to modify the library to read from a database
 You could just wrap it in a monad and for testing purposes make the monad a list or Map

 In order to calculate the correct payment we need to take into account the day count convention.
This seems to be linked to the market. So we need to work out what market is being used and then work out the day convention
to use. But this should not be a concern of the library. It should be explicityl set by the user. So if e

-}
