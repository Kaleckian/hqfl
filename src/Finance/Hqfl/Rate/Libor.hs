-----------------------------------------------------------------------------
-- |
-- Module      :  Finance.Hqfl.Rate.Libor
-- Copyright   :  (C) 2018 Mika'il Khan
-- License     :  (see the file LICENSE)
-- Maintainer  :  Mika'il Khan <co.category@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
----------------------------------------------------------------------------   
module Finance.Hqfl.Rate.Libor
(
    Maturity,
    Libor
  )
  where

import Finance.Hqfl.Currency

-- see https://www.global-rates.com/interest-rates/interest-rates.aspx
-- see https://www.theice.com/iba/libor

data Maturity = Overnight
              | OneWeek
              | TwoWeeks
              | OneMonth
              | TwoMonths
              | ThreeMonths
              | FourMonths
              | FiveMonths
              | SixMonths
              | SevenMonths
              | EightMonths
              | NineMonths
              | TenMonths
              | ElevenMonths
              | TwelveMonths

data Libor = Libor Currency Maturity
