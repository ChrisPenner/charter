{-# LANGUAGE OverloadedStrings #-}
module Main where

import Charts
import Examples.Histogram
import Examples.Random
import Examples.LineChart
import Examples.Auto

-- main :: IO ()
-- main = serveChart 8080 myAutoChart


main :: IO ()
main = serveDynamicChart 8080 buildRandomChart
