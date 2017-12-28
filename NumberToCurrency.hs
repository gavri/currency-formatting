{-# LANGUAGE OverloadedStrings #-}

module NumberToCurrency (convert, convertWithOptions, options,
                        precision, unit, delimiter, separator, format, negativeFormat) where

import Text.Printf
import Data.Text as T hiding (head, last, length)

convertWithOptions num options  = (replace "%u" u . replace "%n" n) chosenFormat
  where n = commafyIntegerPart (pack (printf magnitudeFormat magnitude)) (delimiter options) (separator options)
        u = unit options
        chosenFormat = if (num < 0) then (negativeFormat options) else (format options)
        magnitudeFormat = "%." ++ (show (precision options)) ++ "f"
        magnitude = abs num

convert n = convertWithOptions n defaultOptions

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

defaultOptions = CurrencyFormattingOptions {
  precision = 2
  , unit = "$"
  , delimiter = ","
  , separator = "."
  , format = "%u%n"
  , negativeFormat = "-%u%n"
}

options = defaultOptions
