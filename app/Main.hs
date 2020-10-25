{-# LANGUAGE OverloadedStrings #-}
module Main where

import Charts
import Examples.Histogram
import Examples.Random
import Examples.LineChart
import Examples.Auto
import Examples.ScatterChart
import Examples.BarChart
import Examples.ColumnChart
import Examples.AreaChart

-- main :: IO ()
-- main = serveChart 8080 myAutoChart

main :: IO ()
main = serveChart 8080 areaChart
-- main = serveUpdatingRandomChart
