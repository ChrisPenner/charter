{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Examples.Auto where

import qualified Data.Text as T

import Charts

myData :: [(T.Text, Int, Int)]
myData = [ ("A", 16, 20)
         , ("B", 11, 23)
         , ("C", 9, 25)
         , ("D", 8, 34)
         ]


myAutoChart :: Chart
myAutoChart = autoChart (defaultChartOptions{style=BarChart}) myData
