{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit

import Text.Printf
import Data.Text as T

commafy n = T.reverse $  intercalate "," (chunksOf 3 (T.reverse n))
commafyIntegerPart n = T.concat [(commafy integerPart), ".", fractionalPart]
        where (integerPart: fractionalPart: _) = splitOn "." n

numberToCurrency :: (Ord a, Num a, PrintfArg a) => a -> Text
numberToCurrency n = T.concat [(if isNegative then "-" else ""), commafyIntegerPart $ T.pack (printf "$%.2f" (abs n))]
        where isNegative = n < 0

main = runTestTT $ test $ [
  "simple" ~: "$5.12" ~=? numberToCurrency(5.12 :: Double)
  , "significant digits" ~: "$5.00" ~=? numberToCurrency(5 :: Double)
  , "decimal rounding" ~: "$5.12" ~=? numberToCurrency(5.117 :: Double)
  , "commas" ~: "$1,234,567,890.50" ~=? numberToCurrency(1234567890.50 :: Double)
  , "negative" ~: "-$1,234,567,890.50" ~=? numberToCurrency(-1234567890.50 :: Double)]
