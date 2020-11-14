{-# LANGUAGE OverloadedStrings #-}
module Examples.BarChart where

import Charts
import qualified Data.Text as T
import Data.Aeson

chartColumns :: [Column]
chartColumns = [StringColumn "Year", NumberColumn "Attendance",  AnnotationColumn, NumberColumn "Other", AnnotationColumn]

barChart :: Chart
barChart = autoChartWithHeaders chartOptions BarChart chartColumns chartData
  where
    chartOptions = ChartOptions $ object ["title" .= ("Attendance Chart" :: T.Text)]

-- chartData :: [(T.Text, Int, T.Text, Int, T.Text)]
chartData =
    [ [ "2004", Number 1000, "A'", Number 400, "A" ]
    , [ "2005", Number 1170, "B'", Number 460, "B" ]
    , [ "2006", Number 660, "C'", Number 1120, "C" ]
    , [ "2007", Number 1030, "D'", Number 540, "D" ]
    ]

-- t :: Chart
-- t = buildChart defaultChartOptions BarChart
--   [StringColumn "Year", NumberColumn "Population"]
--   [ [ String "2004", Number 1000 ]
--   , [ String "2005", Number 1170 ]
--   , [ String "2006", Number 660 ]
--   , [ String "2007", Number 1030 ]
--   ]
