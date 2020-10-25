{-# LANGUAGE OverloadedStrings #-}
module Main where

import Charts
import Examples.Histogram
import Examples.LineChart

main :: IO ()
main = serveChart 8080 lineChart

