{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit

import Text.Printf
import Data.Text as T

numberToCurrency n = T.pack (printf "$%.2f" n)

main = runTestTT $ test $ [
  "simple" ~: "$5.12" ~=? numberToCurrency(5.12 :: Double)
  , "significant digits" ~: "$5.00" ~=? numberToCurrency(5 :: Double)
  , "decimal rounding" ~: "$5.12" ~=? numberToCurrency(5.117 :: Double)]
