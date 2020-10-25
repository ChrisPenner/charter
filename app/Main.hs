{-# LANGUAGE OverloadedStrings #-}
module Main where

import Charts
import Examples.Histogram
import Examples.Random
import Examples.LineChart
-- import Examples.Auto
import qualified Data.Text as T

-- main :: IO ()
-- main = serveChart 8080 myAutoChart



main :: IO ()
main = serveChart 8080 . autoChart (defaultChartOptions{style=BarChart}) $ myData
  where
    myData :: [(T.Text, Int, Int)]
    myData = [ ("A", 16, 20)
            , ("B", 11, 23)
            , ("C", 9, 25)
            , ("D", 8, 34)
            ]
