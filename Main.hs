{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit

import Text.Printf
import Data.Text as T

commafy n = T.reverse $  intercalate "," (chunksOf 3 (T.reverse n))
commafyIntegerPart n = T.concat [(commafy integerPart), ".", fractionalPart]
        where (integerPart: fractionalPart: _) = splitOn "." n

numberToCurrency :: (PrintfArg a) => a -> Text
numberToCurrency n = commafyIntegerPart $ T.pack (printf "$%.2f" n)

main = runTestTT $ test $ [
  "simple" ~: "$5.12" ~=? numberToCurrency(5.12 :: Double)
  , "significant digits" ~: "$5.00" ~=? numberToCurrency(5 :: Double)
  , "decimal rounding" ~: "$5.12" ~=? numberToCurrency(5.117 :: Double)
  , "commas" ~: "$1,234,567,890.50" ~=? numberToCurrency(1234567890.50 :: Double)]
