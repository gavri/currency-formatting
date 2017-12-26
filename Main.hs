{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit

import Text.Printf
import Data.Text as T

commafy n = T.reverse $  intercalate "," (chunksOf 3 (T.reverse n))
commafyIntegerPart n = case (Prelude.length components) of
  1 -> commafy (Prelude.head components)
  otherwise -> T.concat [(commafy (Prelude.head components)), ".", (Prelude.last components)]
  where components = splitOn "." n

data CurrencyFormattingOptions = CurrencyFormattingOptions {
  precision :: Int
}

defaultCurrencyFormattingOptions = CurrencyFormattingOptions {
  precision = 2
}

numberToCurrencyRaw :: (Ord a, Num a, PrintfArg a) => a -> CurrencyFormattingOptions -> Text
numberToCurrencyRaw n options  = T.concat [sign n, commafyIntegerPart $ (T.pack (printf ("$%." ++ (show (precision options)) ++ "f") (abs n)))]

numberToCurrency n = numberToCurrencyRaw n defaultCurrencyFormattingOptions

numberToCurrencyWithOptions n options = numberToCurrencyRaw n options

sign n = if isNegative then "-" else ""
  where isNegative = n < 0

main = runTestTT $ test $ [
  "simple" ~: "$5.12" ~=? numberToCurrency(5.12 :: Double)
  , "significant digits" ~: "$5.00" ~=? numberToCurrency(5 :: Double)
  , "decimal rounding" ~: "$5.12" ~=? numberToCurrency(5.117 :: Double)
  , "commas" ~: "$1,234,567,890.50" ~=? numberToCurrency(1234567890.50 :: Double)
  , "negative" ~: "-$1,234,567,890.50" ~=? numberToCurrency(-1234567890.50 :: Double)
  , "precision" ~: "-$1,234,567,890.507" ~=? numberToCurrencyWithOptions (-1234567890.5067 :: Double) (defaultCurrencyFormattingOptions {precision = 3})
  , "zero precision" ~: "-$1,234,567,891" ~=? numberToCurrencyWithOptions (-1234567890.5067 :: Double) (defaultCurrencyFormattingOptions {precision = 0})]
