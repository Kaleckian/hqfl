-----------------------------------------------------------------------------
-- |
-- Module      :  Finance.Hqfl.Pricer.AsaySpec
-- Copyright   :  (C) 2016 Mika'il Khan
-- License     :  (see the file LICENSE)
-- Maintainer  :  Mika'il Khan <co.kleisli@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
----------------------------------------------------------------------------   

module Finance.Hqfl.Pricer.AsaySpec
  (
    main
  , spec
  ) where

import Test.Hspec
import Finance.Hqfl.Pricer.Asay
import Finance.Hqfl.Instrument

main :: IO ()
main = hspec spec

spec = do

  let tolerance = 0.0001
  
  -- Examples from: "The Complete Guide to Option Pricing Formulas", E.G. Haug, McGraw-Hill 2nd Edition
  
  describe "Asay 1982 Pricer" $ do
    context "Given an option on a future which has its premium fully marginalised" $ do
      it "returns the price of a call" $
        (abs (price (Option (Future 4200) Call European 3800 0.75) 0 0.15) - 465.6185421153573) < tolerance
     
      it "returns the price of a put" $
        (abs (price (Option (Future 4200) Put European 3800 0.75) 0 0.15) - 65.61854211535706) < tolerance
