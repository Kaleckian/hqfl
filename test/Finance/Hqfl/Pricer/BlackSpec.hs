-----------------------------------------------------------------------------
-- |
-- Module      :  Finance.Hqfl.Pricer.BlackSpec
-- Copyright   :  (C) 2016 Mika'il Khan
-- License     :  (see the file LICENSE)
-- Maintainer  :  Mika'il Khan <co.kleisli@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
----------------------------------------------------------------------------   

module Finance.Hqfl.Pricer.BlackSpec
  (
    main
  , spec
  ) where

import Test.Hspec
import Finance.Hqfl.Pricer.Black
import Finance.Hqfl.Instrument

main :: IO ()
main = hspec spec

spec = do

  let tolerance = 0.0001
  
  -- Examples from: "The Complete Guide to Option Pricing Formulas", E.G. Haug, McGraw-Hill 2nd Edition
  
  describe "Black 1976 Pricer" $ do
    context "Given an option on a future" $ do
      it "returns the price of a call" $
        (abs (price (Option (Future 19) Call European 19 0.75) 0.1 0.28) - 1.7011) < tolerance
     
      it "returns the price of a put" $
        (abs (price (Option (Future 19) Put European 19 0.75) 0.1 0.28) - 1.7011) < tolerance
