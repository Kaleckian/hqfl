-----------------------------------------------------------------------------
-- |
-- Module      :  Finance.Hqfl.Convention.BusinessDay
-- Copyright   :  (C) 2018 Mika'il Khan
-- License     :  (see the file LICENSE)
-- Maintainer  :  Mika'il Khan <co.category@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
----------------------------------------------------------------------------   

module Finance.Hqfl.Convention.BusinessDay
  (
    BusinessDay
  )
  where

data BusinessDay = NoAdjustment
                 | Following
                 | Preceding
                 | ModifiedFollowing
                 | ModifiedFollowingBimonthly
                 | EndOfMonthNoAdjustment
                 | EndOfMonthPrevious
                 
