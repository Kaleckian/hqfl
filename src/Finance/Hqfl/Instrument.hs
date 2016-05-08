module Finance.Hqfl.Instrument
       (
         -- Capital Market Assets
         module Finance.Hqfl.Instrument.Equity
       , module Finance.Hqfl.Instrument.Bond

         -- Derivatives
       , module Finance.Hqfl.Instrument.Option
       , module Finance.Hqfl.Instrument.Cap
       , module Finance.Hqfl.Instrument.Commodity
       , module Finance.Hqfl.Instrument.Credit
       , module Finance.Hqfl.Instrument.Currency
       , module Finance.Hqfl.Instrument.Energy
       , module Finance.Hqfl.Instrument.Forward
       , module Finance.Hqfl.Instrument.Future
       , module Finance.Hqfl.Instrument.Index
       , module Finance.Hqfl.Instrument.Insurance
       , module Finance.Hqfl.Instrument.Swap
       , module Finance.Hqfl.Instrument.Weather
       , Instrument(..)
       ) where

import Finance.Hqfl.Instrument.Equity
import Finance.Hqfl.Instrument.Bond
import Finance.Hqfl.Instrument.Option
import Finance.Hqfl.Instrument.Cap
import Finance.Hqfl.Instrument.Commodity
import Finance.Hqfl.Instrument.Credit
import Finance.Hqfl.Instrument.Currency
import Finance.Hqfl.Instrument.Energy
import Finance.Hqfl.Instrument.Forward
import Finance.Hqfl.Instrument.Future
import Finance.Hqfl.Instrument.Index
import Finance.Hqfl.Instrument.Insurance
import Finance.Hqfl.Instrument.Swap
import Finance.Hqfl.Instrument.Weather


data Instrument = Instrument (Option Equity)

