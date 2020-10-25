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

autoChart :: forall row. (ADT row, ChartRow' row) => ChartOptions -> ChartStyle -> [row] -> Chart
autoChart opts style xs@[] = autoChartWithHeaders opts style [] xs
autoChart opts style (x:xs) = autoChartWithHeaders opts style columns (x:xs)
  where
    columns :: [Column]
    columns = gfoldMap @ChartRow toColumn x
    toColumn :: forall a. ChartRow a => a -> [Column]
    toColumn _ = [columnHeader (Proxy @a)]

autoChartWithHeaders :: forall row. (ADT row, Constraints row ToJSON) => ChartOptions -> ChartStyle -> [Column] -> [row] -> Chart
autoChartWithHeaders opts style columns [] = buildChart opts style columns []
autoChartWithHeaders opts style columns (x:xs) = buildChart opts style columns rows
  where
    rows :: [[Value]]
    rows = rowToJSON <$> (x:xs)
    rowToJSON :: row -> [Value]
    rowToJSON row = gfoldMap @ToJSON singleToRow row
    singleToRow :: forall a. ToJSON a => a -> [Value]
    singleToRow = (:[]) . toJSON
