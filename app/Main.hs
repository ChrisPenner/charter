{-# LANGUAGE OverloadedStrings #-}
module Main where

import Charts
import Charts.Internal.Chart

main :: IO ()
main = serveChart 8080 testChart

testColumns :: [Column]
testColumns = [Column "Year" StringColumn, Column "Attendance" NumberColumn, Column "Other" NumberColumn]
testChart :: Chart a
testChart = Chart testColumns testData LineChart (ChartOptions{title="My Chart"})

testData :: [[Cell]]
testData = [ ["2004",  1000,      400]
           , ["2005",  1170,      460]
           , ["2006",  660,       1120]
           , ["2007",  1030,      540]
           ]
