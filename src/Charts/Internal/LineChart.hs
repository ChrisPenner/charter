{-# LANGUAGE DataKinds #-}
module Charts.Internal.LineChart where

import qualified Data.Text as T
import Charts.Internal.Chart
import Data.Maybe

lineChart :: [Column] -> [[Cell]] -> Maybe ChartOptions -> Chart LineChart
lineChart columns cells opts = 
    Chart { columns=columns
          , dataTable=cells
          , chartStyle=LineChart
          , options=fromMaybe mempty opts
          }
