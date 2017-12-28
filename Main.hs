{-# LANGUAGE OverloadedStrings #-}

module NumberToCurrency (convert, convertWithOptions, defaultCurrencyFormattingOptions) where

import Text.Printf
import Data.Text as T hiding (head, last, length)

import Test.HUnit

convertWithOptions num options  = (replace "%u" u . replace "%n" n) chosenFormat
  where n = commafyIntegerPart (pack (printf magnitudeFormat magnitude)) (delimiter options) (separator options)
        u = unit options
        chosenFormat = if (num < 0) then (negativeFormat options) else (format options)
        magnitudeFormat = "%." ++ (show (precision options)) ++ "f"
        magnitude = abs num

convert n = convertWithOptions n defaultCurrencyFormattingOptions

commafyIntegerPart n delimiter separator = if hasFractionalPart
                                             then commafiedIntegerPart `append` separator `append` fractionalPart
                                             else commafiedIntegerPart
  where commafiedIntegerPart = commafy integerPart delimiter
        integerPart = head components
        fractionalPart = last components
        hasFractionalPart = length components /= 1
        components = splitOn "." n

commafy n delimiter = T.reverse $  intercalate delimiter (chunksOf 3 (T.reverse n))

data CurrencyFormattingOptions = CurrencyFormattingOptions {
  precision :: Int
  , unit :: Text
  , delimiter :: Text
  , separator :: Text
  , format :: Text
  , negativeFormat :: Text
}

defaultCurrencyFormattingOptions = CurrencyFormattingOptions {
  precision = 2
  , unit = "$"
  , delimiter = ","
  , separator = "."
  , format = "%u%n"
  , negativeFormat = "-%u%n"
}

main = runTestTT $ test $ [
  "simple" ~: "$5.12" ~=? convert (5.12 :: Double)
  , "significant digits" ~: "$5.00" ~=? convert (5 :: Double)
  , "decimal rounding" ~: "$5.12" ~=? convert (5.117 :: Double)
  , "commas" ~: "$1,234,567,890.50" ~=? convert (1234567890.50 :: Double)
  , "negative" ~: "-$1,234,567,890.50" ~=? convert (-1234567890.50 :: Double)
  , "precision" ~: "-$1,234,567,890.507" ~=? convertWithOptions (-1234567890.5067 :: Double) defaultCurrencyFormattingOptions {precision = 3}
  , "zero precision" ~: "-$1,234,567,891" ~=? convertWithOptions (-1234567890.5067 :: Double) defaultCurrencyFormattingOptions {precision = 0}
  , "unit" ~: "£5.12" ~=? convertWithOptions (5.12 :: Double) defaultCurrencyFormattingOptions {unit = "£"}
  , "delimiter" ~: "$1;234;567;890.50" ~=? convertWithOptions (1234567890.50 :: Double) defaultCurrencyFormattingOptions {delimiter = ";"}
  , "delimiter and zero precision" ~: "$1;234;567;891" ~=? convertWithOptions (1234567890.5067 :: Double) defaultCurrencyFormattingOptions {delimiter = ";", precision = 0}
  , "separator" ~: "$1,234,567,890;50" ~=? convertWithOptions (1234567890.50 :: Double) defaultCurrencyFormattingOptions {separator = ";"}
  , "format" ~: "1,234,567,890.50 €" ~=? convertWithOptions (1234567890.50 :: Double) defaultCurrencyFormattingOptions {format = "%n %u", unit = "€"}
  , "negativeFormat" ~: "($1,234,567,890.50)" ~=? convertWithOptions (-1234567890.50 :: Double) defaultCurrencyFormattingOptions {negativeFormat = "(%u%n)"}]
