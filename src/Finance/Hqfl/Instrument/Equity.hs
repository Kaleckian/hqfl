-----------------------------------------------------------------------------
-- |
-- Module      :  Finance.Hqfl.Instrument.Equity
-- Copyright   :  (C) 2016 Mika'il Khan
-- License     :  (see the file LICENSE)
-- Maintainer  :  Mika'il Khan <co.kleisli@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
----------------------------------------------------------------------------   

module Finance.Hqfl.Instrument.Equity
  where

import Finance.Hqfl.Instrument.Type

data Equity = Equity Price Dividend deriving (Show)
