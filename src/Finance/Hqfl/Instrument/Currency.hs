-----------------------------------------------------------------------------
-- |
-- Module      :  Finance.Hqfl.Instrument.Currency
-- Copyright   :  (C) 2016 Mika'il Khan
-- License     :  (see the file LICENSE)
-- Maintainer  :  Mika'il Khan <co.kleisli@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
----------------------------------------------------------------------------

module Finance.Hqfl.Instrument.Currency
  where

import Finance.Hqfl.Instrument.Type

data Currency = Currency Price Rate deriving (Show)
