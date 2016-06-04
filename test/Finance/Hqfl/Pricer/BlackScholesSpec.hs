module Finance.Hqfl.Pricer.BlackScholesSpec
  (
    main
  , spec
  ) where

import Test.Hspec
import Finance.Hqfl.Pricer.BlackScholes
import Finance.Hqfl.Instrument

main :: IO ()
main = hspec spec

spec = do
  describe "Black Scholes Pricer" $ do
    it "returns the price of a vanilla call option" $ do
      blackscholesprice (Option (Equity 42) Call European 40 0.5) 0.1 0.2 `shouldBe` 4.759422392871535
     
    it "returns the price of a vanilla put option" $ do
      blackscholesprice (Option (Equity 42) Put European 40 0.5) 0.1 0.2 `shouldBe` 0.8085993729000958
