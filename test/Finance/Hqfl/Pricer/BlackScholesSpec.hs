module Finance.Hqfl.Pricer.BlackScholesSpec
  (
    main
  , spec
  ) where

import Test.Hspec
import Finance.Hqfl.Pricer.BlackScholes
import Finance.Hqfl.Instrument
import Data.Either

main :: IO ()
main = hspec spec

spec = do
  describe "Black Scholes Pricer" $ do
    it "returns the price of a vanilla call option" $ do
      blackscholesprice (Option (Equity 42) Call European 40 0.5 0) 0.1 0.2 `shouldBe` Right 4.759422392871535
     
    it "returns the price of a vanilla put option" $ do
      blackscholesprice (Option (Equity 42) Put European 40 0.5 0) 0.1 0.2 `shouldBe` Right 0.8085993729000958

    context "Given a Down-and-In European Call Equity Barrier Option" $ do
      context "When the stock price is less than the barrier" $ do
        it "the contract is converted to a vanilla call" $ do
          blackscholesprice (BarrierOption (Equity 80) DownAndIn European Call 90 100 1.0 0.05) 0.1 0.15 `shouldBe` Left "Contract is converted to a vanilla call"

      context "When the barrier price is less than the strike" $ do
        it "the price is calculated" $ do
          blackscholesprice (BarrierOption (Equity 100) DownAndIn European Call 90 100 1.0 0.05) 0.1 0.15 `shouldBe` Right 0.6312064114470974
          
      context "When the barrier price is greater than the strike" $ do
        it "the price is calculated" $ do
          blackscholesprice (BarrierOption (Equity 100) DownAndIn European Call 95 90 1.0 0.05) 0.1 0.15 `shouldBe` Right 6.275436676978629

    context "Given a Down-and-Out European Call Equity Barrier Option" $ do
      context "When the stock price is less than the barrier" $ do
        it "the contract is voided" $ do
          blackscholesprice (BarrierOption (Equity 80) DownAndOut European Call 90 100 1.0 0.05) 0.1 0.15 `shouldBe` Left "Contract is void"

      context "When the barrier price is less than the strike" $ do
        it "the price is calculated" $ do
          blackscholesprice (BarrierOption (Equity 100) DownAndOut European Call 90 100 1.0 0.05) 0.1 0.15 `shouldBe` Right 7.541431780268267
          
      context "When the barrier price is greater than the strike" $ do
        it "the price is calculated" $ do
          blackscholesprice (BarrierOption (Equity 100) DownAndOut European Call 95 90 1.0 0.05) 0.1 0.15 `shouldBe` Right 8.437380137422835
          
    context "Given a Up-and-In European Call Equity Barrier Option" $ do
      context "When the stock price is less than the barrier" $ do
        it "the contract is converted to a vanilla call" $ do
          blackscholesprice (BarrierOption (Equity 100) UpAndIn European Call 90 100 1 0.05) 0.10  0.15 `shouldBe` Left "Contract is converted to a vanilla call"

      context "When the barrier price is less than the strike" $ do
        it "the price is calculated" $ do
          blackscholesprice (BarrierOption (Equity 60) UpAndIn European Call 90 100 1 0.05) 0.10  0.15 `shouldBe` Right 3.177304025489283e-3
          
      context "When the barrier price is greater than the strike" $ do
        it "the price is calculated" $ do
          blackscholesprice (BarrierOption (Equity 60) UpAndIn European Call 90 70 1 0.05) 0.10  0.15 `shouldBe` Right 0.2527658929608891
                    
    context "Given a Down-and-In European Put Equity Barrier Option" $ do
      context "When the stock price is less than the barrier" $ do
        it "the contract is converted to a vanilla call" $ do
          blackscholesprice (BarrierOption (Equity 80) DownAndIn European Put 90 100 1 0.05) 0.10  0.15 `shouldBe` Left "Contract is converted to a vanilla call"

      context "When the barrier price is less than the strike" $ do
        it "the price is calculated" $ do
          blackscholesprice (BarrierOption (Equity 100) DownAndIn European Put 90 100 1 0.05) 0.10  0.15 `shouldBe` Right 3.255754346302549
          
      context "When the barrier price is greater than the strike" $ do
        it "the price is calculated" $ do
          blackscholesprice (BarrierOption (Equity 100) DownAndIn European Put 90 80 1 0.05) 0.10  0.15 `shouldBe` Right 0.1681659314048356
          
    context "Given a Down-and-Out European Put Equity Barrier Option" $ do
      context "When the stock price is less than the barrier" $ do
        it "it is converted to a vanilla call" $ do
          blackscholesprice (BarrierOption (Equity 80) DownAndOut European Put 90 100 1 0.05) 0.10  0.15 `shouldBe` Left "Contract is void"

      context "When the barrier price is less than the strike" $ do
        it "the barrier option is pricexbxbd" $ do
          blackscholesprice (BarrierOption (Equity 100) DownAndOut European Put 90 100 1 0.05) 0.10  0.15 `shouldBe` Right 0.27768319893735693
          
      context "When the barrier price is greater than the strike" $ do
        it "the price is calculated" $ do
          blackscholesprice (BarrierOption (Equity 100) DownAndOut European Put 90 80 1 0.05) 0.10  0.15 `shouldBe` Right 0.0

    context "Given a Up-and-In European Put Equity Barrier Option" $ do
      context "When the stock price is less than the barrier" $ do
        it "it is converted to a vanilla call" $ do
          blackscholesprice (BarrierOption (Equity 100) UpAndIn European Put 90 100 1 0.05) 0.10 0.15 `shouldBe` Left "Contract is converted to a vanilla call"

      context "When the barrier price is less than the strike" $ do
        it "the price is calculated" $ do
          blackscholesprice (BarrierOption (Equity 80) UpAndIn European Put 90 100 1 0.05) 0.10 0.15 `shouldBe` Right 4.289037317247146
          
      context "When the barrier price is greater than the strike" $ do
        it "the price is calculated" $ do
          blackscholesprice (BarrierOption (Equity 80) UpAndIn European Put 95 90 1 0.05) 0.10 0.15 `shouldBe` Right 0.32670213171825235
          
    context "Given a Up-and-Out European Put Equity Barrier Option" $ do
      context "When the stock price is less than the barrier" $ do
        it "it is converted to a vanilla call" $ do
          blackscholesprice (BarrierOption (Equity 100) UpAndOut European Put 90 100 1 0.05) 0.10 0.15 `shouldBe` Left "Contract is void"

      context "When the barrier price is less than the strike" $ do
        it "the price is calculated" $ do
          blackscholesprice (BarrierOption (Equity 80) UpAndOut European Put 90 100 1 0.05) 0.10 0.15 `shouldBe` Right 10.861264375272047
          
      context "When the barrier price is greater than the strike" $ do
        it "the price is calculated" $ do
          blackscholesprice (BarrierOption (Equity 80) UpAndOut European Put 95 90 1 0.05) 0.10 0.15 `shouldBe` Right 7.52257863686332
          
