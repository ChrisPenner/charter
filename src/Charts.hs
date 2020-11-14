module Charts ( 
              -- * Serving Charts
                serveChart
              , serveDynamicChart
              -- * Creating Charts
              -- ** Automatic Charts
              , autoChart
              , autoChartWithHeaders
              -- ** Manual Charts
              , buildChart
              -- * Chart Options
              , defaultChartOptions
              , ChartOptions(..)

              -- * Assorted Types
              , Chart
              , Column(..)
              , ChartStyle(..)
              , ChartRowAuto
              , ChartRowHeaderAuto
              , ChartColumn(..)
    ) where

import Charts.Internal.Server
import Charts.Internal.Chart
import Charts.Internal.Auto

