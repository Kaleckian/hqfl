-----------------------------------------------------------------------------
-- |
-- Module      :  Finance.Hqfl.Convention.DayCount
-- Copyright   :  (C) 2018 Mika'il Khan
-- License     :  (see the file LICENSE)
-- Maintainer  :  Mika'il Khan <co.category@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
----------------------------------------------------------------------------   

module Finance.Hqfl.Convention.DayCount
  (
    DayCountConvention
  )
  where

data DayCountConvention = OneToOne
                        | Thirty360
                        | ThirtyE360
                        | ThirtyE360ISDA
                        | ThirtyEPlus350ISA
                        | Act360
                        | Act365Fixed
                        | Act365L
                        | Act365A
                        | NL365
                        | ActActISA
                        | ActActICMA
                        | Business252

-- TODO: date calculation functions
