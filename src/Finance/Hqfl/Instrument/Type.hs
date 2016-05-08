-----------------------------------------------------------------------------
-- |
-- Module      :  Finance.Hqfl.Instrument.Type
-- Copyright   :  (C) 2016 Mika'il Khan
-- License     :  ??
-- Maintainer  :  Mika'il Khan <co.kleisli@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------

module Finance.Hqfl.Instrument.Type
  (
    Price
  , Strike
  , Maturity
  , Value
  ) where
  
import Data.Decimal

type Price = Decimal
type Strike = Decimal
type Maturity = Decimal
type Value = Decimal
