{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
module Charts.Internal.Auto where

import Charts.Internal.Chart
import Generics.OneLiner
import Data.Aeson
import Data.Proxy
import qualified Data.Text as T

class (ToJSON a) => ChartRow a where
  columnHeader :: Proxy a -> Column

instance ChartRow Int where
  columnHeader _ = NumberColumn ""
instance ChartRow T.Text where
  columnHeader _ = StringColumn ""

type ChartRow' a = (ADT a, Constraints a ChartRow, Constraints a ToJSON)

autoChart :: forall row. (ADT row, ChartRow' row) => ChartOptions -> [row] -> Chart
autoChart opts xs@[] = autoChartWithHeaders opts [] xs
autoChart opts (x:xs) = autoChartWithHeaders opts columns (x:xs)
  where
    columns :: [Column]
    columns = gfoldMap @ChartRow toColumn x
    toColumn :: forall a. ChartRow a => a -> [Column]
    toColumn _ = [columnHeader (Proxy @a)]

autoChartWithHeaders :: forall row. (ADT row, Constraints row ToJSON) => ChartOptions -> [Column] -> [row] -> Chart
autoChartWithHeaders opts columns [] = buildChart opts columns []
autoChartWithHeaders opts columns (x:xs) = buildChart opts columns rows
  where
    rows :: [[Value]]
    rows = rowToJSON <$> (x:xs)
    rowToJSON :: row -> [Value]
    rowToJSON row = gfoldMap @ToJSON singleToRow row
    singleToRow :: forall a. ToJSON a => a -> [Value]
    singleToRow = (:[]) . toJSON
