module Finance.Hqfl.Instrument.StockIndex
  (
    StockIndex(..)
  ) where
import Finance.Hqfl.Instrument.Type

data StockIndex = StockIndex Price Dividend deriving (Show)
