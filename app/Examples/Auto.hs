{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Examples.Auto where

import qualified Data.Text as T
import Data.Aeson

import Charts

myAutoChart :: Chart
myAutoChart = autoChartWithHeaders defaultChartOptions BarChart headers myData
  where
    headers :: [Column]
    headers = [StringColumn "Series", NumberColumn "Successes", NumberColumn "Inconclusive"]
    myData :: [(T.Text, Float, Int)]
    myData = [ ("A", 16, 20)
             , ("B", 11, 23)
             , ("C", 9, 25)
             , ("D", 8, 34)
             ]



