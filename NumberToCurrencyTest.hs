{-# LANGUAGE OverloadedStrings #-}

import NumberToCurrency

import Test.HUnit

main = runTestTT $ test $ [
  "simple" ~: "$5.12" ~=? convert (5.12 :: Double)
  , "significant digits" ~: "$5.00" ~=? convert (5 :: Double)
  , "decimal rounding" ~: "$5.12" ~=? convert (5.117 :: Double)
  , "commas" ~: "$1,234,567,890.50" ~=? convert (1234567890.50 :: Double)
  , "negative" ~: "-$1,234,567,890.50" ~=? convert (-1234567890.50 :: Double)
  , "precision" ~: "-$1,234,567,890.507" ~=? convertWithOptions (-1234567890.5067 :: Double) options {precision = 3}
  , "zero precision" ~: "-$1,234,567,891" ~=? convertWithOptions (-1234567890.5067 :: Double) options {precision = 0}
  , "unit" ~: "£5.12" ~=? convertWithOptions (5.12 :: Double) options {unit = "£"}
  , "delimiter" ~: "$1;234;567;890.50" ~=? convertWithOptions (1234567890.50 :: Double) options {delimiter = ";"}
  , "delimiter and zero precision" ~: "$1;234;567;891" ~=? convertWithOptions (1234567890.5067 :: Double) options {delimiter = ";", precision = 0}
  , "separator" ~: "$1,234,567,890;50" ~=? convertWithOptions (1234567890.50 :: Double) options {separator = ";"}
  , "format" ~: "1,234,567,890.50 €" ~=? convertWithOptions (1234567890.50 :: Double) options {format = "%n %u", unit = "€"}
  , "negativeFormat" ~: "($1,234,567,890.50)" ~=? convertWithOptions (-1234567890.50 :: Double) options {negativeFormat = "(%u%n)"}]
