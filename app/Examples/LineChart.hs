{-# LANGUAGE OverloadedStrings #-}
module Examples.LineChart where

import Charts
import Data.Aeson
import qualified Data.Text as T

chartColumns :: [Column]
chartColumns = [StringColumn "Year", NumberColumn "Attendance", NumberColumn "Other", AnnotationColumn]

lineChart :: Chart a
lineChart = Chart chartColumns chartData (ChartOptions{title="My Chart", style=LineChart})

chartData :: [[Value]]
chartData = asJSON <$> 
           [ ("2004",  1000,      400, "A")
           , ("2005",  1170,      460, "B")
           , ("2006",  660,       1120, "C")
           , ("2007",  1030,      540, "D")
           ]
  where
    asJSON :: (T.Text, Int, Int, T.Text) -> [Value]
    asJSON (a, b, c, d) = [toJSON a, toJSON b, toJSON c, toJSON d]
