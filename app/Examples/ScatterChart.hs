{-# LANGUAGE OverloadedStrings #-}
module Examples.ScatterChart where

import Charts

scatterChart :: Chart
scatterChart = autoChartWithHeaders (ChartOptions{title="Age vs. Weight"}) ScatterChart chartColumns chartData

chartColumns :: [Column]
chartColumns = [NumberColumn "Age", NumberColumn "Weight"]

chartData :: [(Float, Float)]
chartData =
    [ (8, 12)
    , (4, 5.5)
    , (11, 14)
    , (4, 5)
    , (3, 3.5)
    , (6.5, 7)
    ]
