{-# LANGUAGE OverloadedStrings #-}

module NumberToCurrency (convert, convertWithOptions, defaultCurrencyFormattingOptions) where

import Text.Printf
import Data.Text as T

import Test.HUnit

convertWithOptions :: (Ord a, Num a, PrintfArg a) => a -> CurrencyFormattingOptions -> Text
convertWithOptions n options  = (replace "%u" (unit options) . replace "%n" magnitude) chosenFormat
  where magnitude = commafyIntegerPart (T.pack (printf ("%." ++ (show (precision options)) ++ "f") (abs n))) (delimiter options) (separator options)
        chosenFormat = if (n < 0) then (negativeFormat options) else (format options)

convert n = convertWithOptions n defaultCurrencyFormattingOptions

commafy n delimiter = T.reverse $  intercalate delimiter (chunksOf 3 (T.reverse n))
commafyIntegerPart n delimiter separator = case (Prelude.length components) of
  1 -> commafiedIntegerPart
  otherwise -> T.concat [commafiedIntegerPart, separator, (Prelude.last components)]
  where components = splitOn "." n
        commafiedIntegerPart = commafy (Prelude.head components) delimiter

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
