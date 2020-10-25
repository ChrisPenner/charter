{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Charts.Internal.Chart where

import qualified Data.Text as T
import Data.Aeson as A
import Data.String

-- https://developers.google.com/chart/interactive/docs/roles#what-roles-are-available
data Role =
        Annotation
      | AnnotationText
      | Certainty
      | Emphasis
      | Interval
      | Scope
      | Style
      | Tooltip
      | Domain
      | Data
  deriving (Show, Eq, Ord)

data ColumnType =
    NumberColumn
      | StringColumn
      | BoolColumn
      | DateColumn
      | DateTimeColumn
      | TimeOfDayColumn

-- Types from https://developers.google.com/chart/interactive/docs/reference#DataTable_addColumn
instance ToJSON ColumnType where
  toJSON = \case
    NumberColumn -> String "number"
    StringColumn -> String "string"
    BoolColumn -> String "boolean"
    DateColumn -> String "date"
    DateTimeColumn -> String "datetime"
    TimeOfDayColumn -> String "timeofday"


data NumberFormatting

-- data Continuity = Discrete | Continuous
--   deriving (Show, Eq, Ord)

data Cell =
    NumberCell Double
      | StringCell T.Text

instance ToJSON Cell where
  toJSON = \case
    NumberCell d -> toJSON d
    StringCell s -> toJSON s

instance IsString Cell where
    fromString = StringCell . T.pack

instance Fractional Cell where
    fromRational = NumberCell . fromRational
    recip = error "unimplemented"
instance Num Cell where
  fromInteger = NumberCell . fromInteger
  (+) = error "unimplemented"
  (*) = error "unimplemented"
  signum = error "unimplemented"
  abs = error "unimplemented"
  negate = error "unimplemented"

data Column = Column
    { label :: T.Text
    , typ :: ColumnType
    -- , continuity :: Continuity
    }

instance ToJSON Column where
  toJSON Column{..} = 
      object [ "label" .= label
             , "type" .= typ
             ]


data ChartStyle = LineChart
data ChartOptions = ChartOptions
    { title :: T.Text
    }

instance ToJSON ChartOptions where
  toJSON ChartOptions{..} =
      A.object [ "title" .= title
               ]

instance Semigroup ChartOptions where
  (<>) = const

instance Monoid ChartOptions where
  mempty = ChartOptions {title=""}

data Chart (style :: ChartStyle) =
    Chart { columns :: [Column]
          , dataTable :: [[Cell]]
          , chartStyle :: ChartStyle
          , options :: ChartOptions
          }

instance ToJSON (Chart a) where
  toJSON (Chart{..}) =
      object [ "rows" .= dataTable
             , "options" .= options
             , "columns" .= columns
             ]

