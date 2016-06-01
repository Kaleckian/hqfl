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
    Barrier
  , Maturity
  , Price
  , Strike
  , Value
  , Type(..)
  , Style(..)
) where
  
type Barrier = Double
type Maturity = Double
type Price = Double
type Strike = Double
type Value = Double

data Type = Call | Put deriving (Eq, Show)
data Style = European | American deriving (Eq, Show)
