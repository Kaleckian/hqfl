-----------------------------------------------------------------------------
-- |
-- Module      :  Finance.Hqfl.Instrument.Futures
-- Copyright   :  (C) 2016 Mika'il Khan
-- License     :  (see the file LICENSE)
-- Maintainer  :  Mika'il Khan <co.kleisli@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
----------------------------------------------------------------------------   

module Finance.Hqfl.Instrument.Future where

import Finance.Hqfl.Instrument.Type

data Future = Future Price deriving (Show)
