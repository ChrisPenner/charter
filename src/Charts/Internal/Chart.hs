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

-- https://developers.google.com/chart/interactive/docs/roles#what-roles-are-available
-- data Role =
--         Annotation
--       | AnnotationText
--       | Certainty
--       | Emphasis
--       | Interval
--       | Scope
--       | Style
--       | Tooltip
--       | Domain
--       | Data
--   deriving (Show, Eq, Ord)


data NumberFormatting

data Column =
        NumberColumn T.Text
      | StringColumn T.Text
      | BoolColumn T.Text
      | DateColumn T.Text
      | DateTimeColumn T.Text
      | TimeOfDayColumn T.Text
-- https://developers.google.com/chart/interactive/docs/roles?hl=en#what-roles-are-available
      | AnnotationColumn
      | AnnotationTextColumn
      | CertaintyColumn
      | EmphasisColumn
      | IntervalColumn
      | ScopeColumn
      | StyleColumn
      | TooltipColumn
    -- Shouldn't need to use these explicitly in most cases
      | DomainColumn
      | DataColumn

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

data ChartStyle = LineChart
                | Histogram
                | BarChart
                  deriving (Show, Eq, Ord)

instance ToJSON ChartStyle where
  toJSON = \case
    LineChart -> "line"
    Histogram -> "histogram"
    BarChart -> "bar"

data ChartOptions = ChartOptions
    { title :: T.Text
    , style :: ChartStyle
    }

instance ToJSON ChartOptions where
  toJSON ChartOptions{..} =
      A.object [ "title" .= title
                , "style" .= style
               ]

instance Semigroup ChartOptions where
  (<>) = const

instance Monoid ChartOptions where
  mempty = ChartOptions {title="", style=LineChart}

data Chart (style :: ChartStyle) =
    Chart { columns :: [Column]
          , dataTable :: [[Value]]
          , options :: ChartOptions
          }

instance ToJSON (Chart a) where
  toJSON (Chart{..}) =
      object [ "rows" .= dataTable
             , "options" .= options
             , "columns" .= columns
             ]

