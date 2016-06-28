-----------------------------------------------------------------------------
-- |
-- Module      :  Finance.Hqfl.Instrument.Option
-- Copyright   :  (C) 2016 Mika'il Khan
-- License     :  (see the file LICENSE)
-- Maintainer  :  Mika'il Khan <co.kleisli@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
----------------------------------------------------------------------------   

module Finance.Hqfl.Instrument.Option
 (
   Option (..)
 )
where

import Finance.Hqfl.Instrument.Type

data Option a = Option a Type Style Strike Expiration deriving (Show)
