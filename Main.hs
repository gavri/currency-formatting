{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit

import Text.Printf
import Data.Text as T

commafy n delimiter = T.reverse $  intercalate delimiter (chunksOf 3 (T.reverse n))
commafyIntegerPart n delimiter separator = case (Prelude.length components) of
  1 -> commafy (Prelude.head components) delimiter
  otherwise -> T.concat [(commafy (Prelude.head components) delimiter), separator, (Prelude.last components)]
  where components = splitOn "." n

data CurrencyFormattingOptions = CurrencyFormattingOptions {
  precision :: Int
  , unit :: Text
  , delimiter :: Text
  , separator :: Text
  , format :: Text
  , negative_format :: Text
}

defaultCurrencyFormattingOptions = CurrencyFormattingOptions {
  precision = 2
  , unit = "$"
  , delimiter = ","
  , separator = "."
  , format = "%u%n"
  , negative_format = "-%u%n"
}

numberToCurrencyRaw :: (Ord a, Num a, PrintfArg a) => a -> CurrencyFormattingOptions -> Text
numberToCurrencyRaw n options  = replace "%u" (unit options) (replace "%n" magnitude format)
  where magnitude = commafyIntegerPart (T.pack (printf ("%." ++ (show (precision options)) ++ "f") (abs n))) (delimiter options) (separator options)
        format = formatFor n options

numberToCurrency n = numberToCurrencyRaw n defaultCurrencyFormattingOptions

numberToCurrencyWithOptions n options = numberToCurrencyRaw n options

formatFor n options = if isNegative then (negative_format options) else (format options)
  where isNegative = n < 0

main = runTestTT $ test $ [
  "simple" ~: "$5.12" ~=? numberToCurrency(5.12 :: Double)
  , "significant digits" ~: "$5.00" ~=? numberToCurrency(5 :: Double)
  , "decimal rounding" ~: "$5.12" ~=? numberToCurrency(5.117 :: Double)
  , "commas" ~: "$1,234,567,890.50" ~=? numberToCurrency(1234567890.50 :: Double)
  , "negative" ~: "-$1,234,567,890.50" ~=? numberToCurrency(-1234567890.50 :: Double)
  , "precision" ~: "-$1,234,567,890.507" ~=? numberToCurrencyWithOptions (-1234567890.5067 :: Double) (defaultCurrencyFormattingOptions {precision = 3})
  , "zero precision" ~: "-$1,234,567,891" ~=? numberToCurrencyWithOptions (-1234567890.5067 :: Double) (defaultCurrencyFormattingOptions {precision = 0})
  , "unit" ~: "£5.12" ~=? numberToCurrencyWithOptions (5.12 :: Double) defaultCurrencyFormattingOptions {unit = "£"}
  , "delimiter" ~: "$1;234;567;890.50" ~=? numberToCurrencyWithOptions (1234567890.50 :: Double) defaultCurrencyFormattingOptions {delimiter = ";"}
  , "delimiter and zero precision" ~: "$1;234;567;891" ~=? numberToCurrencyWithOptions (1234567890.5067 :: Double) defaultCurrencyFormattingOptions {delimiter = ";", precision = 0}
  , "separator" ~: "$1,234,567,890;50" ~=? numberToCurrencyWithOptions (1234567890.50 :: Double) defaultCurrencyFormattingOptions {separator = ";"}
  , "format" ~: "1,234,567,890.50 €" ~=? numberToCurrencyWithOptions (1234567890.50 :: Double) defaultCurrencyFormattingOptions {format = "%n %u", unit = "€"}
  , "negative_format" ~: "($1,234,567,890.50)" ~=? numberToCurrencyWithOptions (-1234567890.50 :: Double) defaultCurrencyFormattingOptions {negative_format = "(%u%n)"}]
