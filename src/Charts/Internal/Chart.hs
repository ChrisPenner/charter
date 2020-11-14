{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Charts.Internal.Chart where

import qualified Data.Text as T
import Data.Aeson as A

-- | Valid Column types. Each "data" column also accepts a column header.
-- "Role" columns do not.
--
-- See https://developers.google.com/chart/interactive/docs/roles
data Column =
        NumberColumn T.Text
      | StringColumn T.Text
      | BoolColumn T.Text
      | DateColumn T.Text
      | DateTimeColumn T.Text
      | TimeOfDayColumn T.Text
      | AnnotationColumn
      | AnnotationTextColumn
      | CertaintyColumn
      | EmphasisColumn
      | IntervalColumn
      | ScopeColumn
      | StyleColumn
      | TooltipColumn
      | DomainColumn
    -- ^ This is typically inferred and explicitly
      | DataColumn
    -- ^ This is typically inferred and explicitly
  deriving (Show, Eq)

-- Types from https://developers.google.com/chart/interactive/docs/reference#DataTable_addColumn
instance ToJSON Column where
  toJSON col =
              case col of
                  StringColumn a -> object [ "label" .= a, "type" ~= "string"]
                  NumberColumn a -> object [ "label" .= a, "type" ~= "number"]
                  BoolColumn a -> object [ "label" .= a, "type" ~= "boolean"]
                  DateColumn a -> object [ "label" .= a, "type" ~= "date"]
                  DateTimeColumn a -> object [ "label" .= a, "type" ~= "datetime"]
                  TimeOfDayColumn a -> object [ "label" .= a, "type" ~= "timeofday"]
                  AnnotationColumn -> object ["role" ~= "annotation"]
                  AnnotationTextColumn -> object ["role" ~= "annotationText"]
                  CertaintyColumn -> object ["role" ~= "certainty"]
                  EmphasisColumn -> object ["role" ~= "emphasis"]
                  IntervalColumn -> object ["role" ~= "interval"]
                  ScopeColumn -> object ["role" ~= "scope"]
                  StyleColumn -> object ["role" ~= "style"]
                  TooltipColumn -> object ["role" ~= "tooltip"]
                  DomainColumn -> object ["role" ~= "domain"]
                  DataColumn -> object ["role" ~= "data"]
    where
      (~=):: T.Text -> T.Text -> (T.Text, Value)
      (~=) = (.=)

-- | Supported chart types
-- See https://developers.google.com/chart/interactive/docs/gallery
data ChartStyle = LineChart
                | Histogram
                | BarChart
                | ColumnChart
                | ScatterChart
                | AreaChart
                | PieChart
                | BubbleChart
                | SteppedAreaChart
                | CandlestickChart
                  deriving (Show, Eq, Ord)

instance ToJSON ChartStyle where
  toJSON = \case
    LineChart -> "line"
    Histogram -> "histogram"
    BarChart -> "bar"
    ColumnChart -> "column"
    ScatterChart -> "scatter"
    AreaChart -> "area"
    PieChart -> "pie"
    BubbleChart -> "bubble"
    SteppedAreaChart -> "steppedarea"
    CandlestickChart -> "candlestick"

-- | I plan to make this typesafe for each partiular chart type
-- but that's a LOT of work, so for now it's just free-form, if you'd actually
-- use this, please make an issue on Github so I know folks need this behaviour :)
--
-- Find your chart in the chart gallery to see which options it will accept.
--
-- https://developers.google.com/chart/interactive/docs/gallery
data ChartOptions = ChartOptions Value

instance ToJSON ChartOptions where
  toJSON (ChartOptions v) = v

-- | Empty chart options
defaultChartOptions :: ChartOptions
defaultChartOptions = ChartOptions (object [])

-- | The primary chart type.
data Chart =
    Chart { options :: ChartOptions
          , style :: ChartStyle
          , columns :: [Column]
          , dataTable :: [[Value]]
          , dynamic :: Bool
          }

instance ToJSON Chart where
  toJSON (Chart{..}) =
      object [ "rows" .= dataTable
             , "options" .= options
             , "columns" .= columns
             , "style" .= style
             , "dynamic" .= dynamic
             ]

-- | Construct a chart.
--
-- e.g.
--
-- @
-- myChart :: Chart
-- myChart = buildChart defaultChartOptions BarChart
--   [StringColumn "Year", NumberColumn "Population"]
--   [ [ String "2004", Number 1000 ]
--   , [ String "2005", Number 1170 ]
--   , [ String "2006", Number 660 ]
--   , [ String "2007", Number 1030 ]
--   ]
-- @
buildChart :: ChartOptions -> ChartStyle -> [Column] -> [[Value]] ->  Chart
buildChart opts style columns vals = Chart opts style columns vals True
