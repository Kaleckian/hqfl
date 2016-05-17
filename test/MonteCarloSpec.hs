import Test.Hspec
import Statistics.Distribution.Normal
import Data.Random

-- Model Based Testing Using Black-Scholes Equation for a vanilla call option
call :: Double -> Double -> Double -> Double -> Double -> Double
call price strike rate time variance =
  (price * (cdf normal d1)) - (strike * (cdf normal d2) * exp ((-rate) * time))
  where d1 =
          (log (price / strike) + (rate * time) + (0.5 * variance * time)) / sqrt(variance * time)
        d2 = d1 - sqrt(variance * time)
        normal = Normal (0 :: Double) 1

--main :: IO ()
--main = d1 95 100  0.10 0.25 0.20
{-
 The model will be the Black-Scholes Equation (you can take modified-}
{-versions of this for the given option type. Then what you need to do-}
{-is to run the monte carlo from the parameters given by the quick-}
{-check out put. Once that is d1, then we can compare. If the-}
{-results are within a given tolerance level then we are good to go

-- TODO first step is to get the blackschole price for a vanilla call-}
{-option
once we have that then we setup the MC simulator likewise - it's like-}
{-that guys paper Numerical SOlution of the Black-SCholes equation-}
{-with a small number of grid points
-}
      
      

  
