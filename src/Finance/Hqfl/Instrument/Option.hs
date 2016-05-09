-----------------------------------------------------------------------------
-- |
-- Module      :  Finance.Hqfl.Instrument.Option
-- Copyright   :  (C) 2016 Mika'il Khan
-- License     :  (see the file LICENSE)
-- Maintainer  :  Mika'il Khan <co.kleisli@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------   

module Finance.Hqfl.Instrument.Option
(
  Option(..),
  Type(..),
  Style(..),
  Exotic(..)
)
where

import Data.Decimal
import Finance.Hqfl.Instrument.Type
import Data.Functor
{-

Classifications:

By Option Rights:

1. Call
2. Put

By Underlying Asserts:

1. Equity Option
2. Bond Option
3. Future Option
4. Index Option
5. Commodity Option
6. Currency Option

By Option Styles

1. American
2. European

American and European are referred to as Vanilla Options

Exotic options are non-Vanilla options:

1. Bermudan
2. Asian
3. Barrier
4. Binary

-}
-- TODO: Refactor into a typeclass?
-- TODO: Function to calculate time to maturity?
-- TODO: Function to get the current price of the underlying
-- TODO: Typeclasses to categorise options according to their pricing function: American, European - each instance to supply pricing method

data Type = Call | Put deriving (Eq, Show)
data Style = European | American deriving (Eq, Show)-- Vanilla
data Exotic = Bermudan | Asian | Barrier | Binary deriving (Eq, Show) -- Exotics

data Option a = Option a Type Style Strike Maturity Value deriving (Show)

-- experimenting; review later
instance Eq a => Eq (Option a) where
  Option a1 b1 c1 d1 e1 f1 == Option a2 b2 c2 d2 e2 f2 = a1 == a2 && b1 == b2 && c1 == c2 && d1 == d2 && e1 == e2 && f1 == f2
    
instance Functor Option  where
  fmap f (Option a b c d e g) = Option (f a) b c d e g

class Option a where
  callPayOff :: Double -> Double
  putPayOff :: Double -> Double

instance Option Equity where
  callPayOff = undefined
  putPayOff = undefined
