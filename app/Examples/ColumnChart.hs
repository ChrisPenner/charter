{-# LANGUAGE OverloadedStrings #-}
module Examples.ColumnChart where

import Charts
import qualified Data.Text as T

chartColumns :: [Column]
chartColumns = [StringColumn "Year", NumberColumn "Attendance",  AnnotationColumn, NumberColumn "Other", AnnotationColumn]

columnChart :: Chart
columnChart = autoChartWithHeaders defaultChartOptions ColumnChart chartColumns chartData

chartData :: [(T.Text, Int, T.Text, Int, T.Text)]
chartData =
    [ ("2004", 1000, "A'", 400, "A")
    , ("2005", 1170, "B'", 460, "B")
    , ("2006", 660, "C'", 1120, "C")
    , ("2007", 1030, "D'", 540, "D")
    ]
