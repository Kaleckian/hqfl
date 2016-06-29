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

  let tolerance = 0.0001
  
  -- Examples from: "The Complete Guide to Option Pricing Formulas", E.G. Haug, McGraw-Hill 2nd Edition
  
  describe "Black Scholes Pricer" $ do
    context "Given an option on a stock" $ do
      it "returns the price of a call" $
        (abs (price (Option (Equity 60 0) Call European 65 0.25) 0.08 0.3) - 2.1334) < tolerance
     
      it "returns the price of a put" $
        (abs (price (Option (Equity 60 0) Put European 65 0.25) 0.08 0.3) - 5.84628) < tolerance

    context "Given an option on a stock index" $ do
      it "returns the price of a call" $
        (abs (price (Option (StockIndex 100 0.05) Call European 95 0.5) 0.1 0.2) - 9.628983522021265) < tolerance
     
      it "returns the price of a put" $
        (abs (price (Option (StockIndex 100 0.05) Put European 95 0.5) 0.1 0.2) - 2.46478764675582) < tolerance

    -- context "Given a Down-and-In European Call Equity Barrier Option" $ do
    --   context "When the stock price is less than the barrier" $ do
    --     it "the contract is converted to a vanilla call" $ do
    --       1 == 1
    --       --(abs (blackscholesprice (BarrierOption (Equity 80) DownAndIn European Call 90 100 1.0 0.05) 0.1 0.15 `shouldBe` Left "Contract is converted to a vanilla call"

    --   context "When the barrier price is less than the strike" $ do
    --     it "the price is calculated" $ do
    --       (abs (blackscholesprice (BarrierOption (Equity 100) DownAndIn European Call 90 100 1.0 0.05) 0.1 0.15) - 0.6312064114470974) < 0.0001
          
    --   context "When the barrier price is greater than the strike" $ do
    --     it "the price is calculated" $ do
    --       (abs (blackscholesprice (BarrierOption (Equity 100) DownAndIn European Call 95 90 1.0 0.05) 0.1 0.15) - 6.275436676978629) < 0.0001

    -- context "Given a Down-and-Out European Call Equity Barrier Option" $ do
    --   context "When the stock price is less than the barrier" $ do
    --     it "the contract is voided" $ do
    --       1 == 1
    --       --(abs (blackscholesprice (BarrierOption (Equity 80) DownAndOut European Call 90 100 1.0 0.05) 0.1 0.15) `shouldBe` Left "Contract is void"

    --   context "When the barrier price is less than the strike" $ do
    --     it "the price is calculated" $ do
    --       (abs (blackscholesprice (BarrierOption (Equity 100) DownAndOut European Call 90 100 1.0 0.05) 0.1 0.15) - 7.541431780268267) < 0.0001
          
    --   context "When the barrier price is greater than the strike" $ do
    --     it "the price is calculated" $ do
    --       (abs (blackscholesprice (BarrierOption (Equity 100) DownAndOut European Call 95 90 1.0 0.05) 0.1 0.15) - 8.437380137422835) < 0.0001
          
    -- context "Given a Up-and-In European Call Equity Barrier Option" $ do
    --   context "When the stock price is less than the barrier" $ do
    --     it "the contract is converted to a vanilla call" $ do
    --       1 == 1
    --       --(abs (blackscholesprice (BarrierOption (Equity 100) UpAndIn European Call 90 100 1 0.05) 0.10  0.15) `shouldBe` Left "Contract is converted to a vanilla call"

    --   context "When the barrier price is less than the strike" $ do
    --     it "the price is calculated" $ do
    --       (abs (blackscholesprice (BarrierOption (Equity 60) UpAndIn European Call 90 100 1 0.05) 0.10  0.15) - 3.177304025489283e-3) < 0.0001
          
    --   context "When the barrier price is greater than the strike" $ do
    --     it "the price is calculated" $ do
    --       (abs (blackscholesprice (BarrierOption (Equity 60) UpAndIn European Call 90 70 1 0.05) 0.10  0.15) - 0.2527658929608891) < 0.0001
                    
    -- context "Given a Down-and-In European Put Equity Barrier Option" $ do
    --   context "When the stock price is less than the barrier" $ do
    --     it "the contract is converted to a vanilla call" $ do
    --       1 == 1
    --       --(abs (blackscholesprice (BarrierOption (Equity 80) DownAndIn European Put 90 100 1 0.05) 0.10  0.15) `shouldBe` Left "Contract is converted to a vanilla call"

    --   context "When the barrier price is less than the strike" $ do
    --     it "the price is calculated" $ do
    --       (abs (blackscholesprice (BarrierOption (Equity 100) DownAndIn European Put 90 100 1 0.05) 0.10  0.15) - 3.255754346302549) < 0.0001
          
    --   context "When the barrier price is greater than the strike" $ do
    --     it "the price is calculated" $ do
    --       (abs (blackscholesprice (BarrierOption (Equity 100) DownAndIn European Put 90 80 1 0.05) 0.10  0.15) - 0.1681659314048356) < 0.0001
          
    -- context "Given a Down-and-Out European Put Equity Barrier Option" $ do
    --   context "When the stock price is less than the barrier" $ do
    --     it "it is converted to a vanilla call" $ do
    --       1 == 1
    --       --(abs (blackscholesprice (BarrierOption (Equity 80) DownAndOut European Put 90 100 1 0.05) 0.10  0.15) `shouldBe` Left "Contract is void"

    --   context "When the barrier price is less than the strike" $ do
    --     it "the barrier option is pricexbxbd" $ do
    --       (abs (blackscholesprice (BarrierOption (Equity 100) DownAndOut European Put 90 100 1 0.05) 0.10  0.15) - 0.27768319893735693) < 0.0001
          
    --   context "When the barrier price is greater than the strike" $ do
    --     it "the price is calculated" $ do
    --       (abs (blackscholesprice (BarrierOption (Equity 100) DownAndOut European Put 90 80 1 0.05) 0.10  0.15) - 0.0) < 0.0001

    -- context "Given a Up-and-In European Put Equity Barrier Option" $ do
    --   context "When the stock price is less than the barrier" $ do
    --     it "it is converted to a vanilla call" $ do
    --       1 == 1
    --       --(abs (blackscholesprice (BarrierOption (Equity 100) UpAndIn European Put 90 100 1 0.05) 0.10 0.15) `shouldBe` Left "Contract is converted to a vanilla call"

    --   context "When the barrier price is less than the strike" $ do
    --     it "the price is calculated" $ do
    --       (abs (blackscholesprice (BarrierOption (Equity 80) UpAndIn European Put 90 100 1 0.05) 0.10 0.15) - 4.289037317247146) < 0.0001
          
    --   context "When the barrier price is greater than the strike" $ do
    --     it "the price is calculated" $ do
    --       (abs (blackscholesprice (BarrierOption (Equity 80) UpAndIn European Put 95 90 1 0.05) 0.10 0.15) - 0.32670213171825235) < 0.0001
          
    -- context "Given a Up-and-Out European Put Equity Barrier Option" $ do
    --   context "When the stock price is less than the barrier" $ do
    --     it "it is converted to a vanilla call" $ do
    --       1 == 1
    --       --(abs (blackscholesprice (BarrierOption (Equity 100) UpAndOut European Put 90 100 1 0.05) 0.10 0.15) `shouldBe` Left "Contract is void"

    --   context "When the barrier price is less than the strike" $ do
    --     it "the price is calculated" $ do
    --       (abs (blackscholesprice (BarrierOption (Equity 80) UpAndOut European Put 90 100 1 0.05) 0.10 0.15) - 10.861264375272047) < 0.0001
          
    --   context "When the barrier price is greater than the strike" $ do
    --     it "the price is calculated" $ do
    --       (abs (blackscholesprice (BarrierOption (Equity 80) UpAndOut European Put 95 90 1 0.05) 0.10 0.15) - 7.52257863686332) < 0.0001
          
