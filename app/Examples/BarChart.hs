{-# LANGUAGE OverloadedStrings #-}
module Examples.BarChart where

import Charts
import qualified Data.Text as T

chartColumns :: [Column]
chartColumns = [StringColumn "Year", NumberColumn "Attendance",  AnnotationColumn, NumberColumn "Other", AnnotationColumn]

barChart :: Chart
barChart = autoChartWithHeaders defaultChartOptions BarChart chartColumns chartData

chartData :: [(T.Text, Int, T.Text, Int, T.Text)]
chartData =
    [ ("2004", 1000, "A'", 400, "A")
    , ("2005", 1170, "B'", 460, "B")
    , ("2006", 660, "C'", 1120, "C")
    , ("2007", 1030, "D'", 540, "D")
    ]
