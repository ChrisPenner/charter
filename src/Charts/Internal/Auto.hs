{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Charts.Internal.Auto where

import Charts.Internal.Chart
import Generics.OneLiner
import Data.Aeson
import Data.Proxy
import qualified Data.Text as T
import Data.Scientific

-- | Class representing types for which a column type can be inferred.
--
-- You can implement your own instances for this if you like, but generally shouldn't need to.
class ChartColumn a where
  columnHeader :: Proxy a -> Column

instance ChartColumn Int where
  columnHeader _ = NumberColumn ""

instance ChartColumn Float where
  columnHeader _ = NumberColumn ""

instance ChartColumn Double where
  columnHeader _ = NumberColumn ""

instance ChartColumn Scientific where
  columnHeader _ = NumberColumn ""

instance ChartColumn T.Text where
  columnHeader _ = StringColumn ""

instance ChartColumn String where
  columnHeader _ = StringColumn ""

instance ChartColumn Bool where
  columnHeader _ = BoolColumn ""

-- | A constraint representing a type which can be converted into a chart row
--
-- This constraint is satisfied by tuple types containing JSON serializable types.
--
-- E.g. data rows of type @(Text, Int, Float)@ will infer a chart with a String column and two number
-- columns.
--
-- You shouldn't need to implement any instances of this constraint yourself.
type ChartRowAuto a = (ADT a, Constraints a ToJSON)

-- | A constraint that a column-type can be determined for all elements of the provided tuple
-- type.
type ChartRowHeaderAuto a = (Constraints a ChartColumn)


-- | Create a chart from the provided options, style, and data by inferring column header
-- types.
--
-- Prefer 'autoChartWithHeaders' when you know your column headers up front.
--
-- E.g. The following generates a 2-series bar chart with 4 sections, A, B, C, D
--
-- @
-- myAutoChart :: Chart
-- myAutoChart = autoChart defaultChartOptions BarChart myData
--   where
--     myData :: [(T.Text, Float, Int)]
--     myData = [ ("A", 16, 20)
--              , ("B", 11, 23)
--              , ("C", 9, 25)
--              , ("D", 8, 34)
--              ]
-- @
autoChart :: forall row. (ChartRowHeaderAuto row, ChartRowAuto row) => ChartOptions -> ChartStyle -> [row] -> Chart
autoChart opts styl xs@[] = autoChartWithHeaders opts styl [] xs
autoChart opts styl (x:xs) = autoChartWithHeaders opts styl cols (x:xs)
  where
    cols :: [Column]
    cols = gfoldMap @ChartColumn toColumn x
    toColumn :: forall a. ChartColumn a => a -> [Column]
    toColumn _ = [columnHeader (Proxy @a)]

-- | Create a chart from the provided options, style, and data, but use the explicitly
-- provided column headers and types.
--
-- E.g. The following generates a 2-series bar chart with 4 sections, A, B, C, D
--
-- @
-- myAutoChart :: Chart
-- myAutoChart = autoChartWithHeaders defaultChartOptions BarChart headers myData
--   where
--     headers :: [Column]
--     headers = [StringColumn "Series", NumberColumn "Successes", NumberColumn "Inconclusive"]
--     myData :: [(T.Text, Float, Int)]
--     myData = [ ("A", 16, 20)
--              , ("B", 11, 23)
--              , ("C", 9, 25)
--              , ("D", 8, 34)
--              ]
-- @
autoChartWithHeaders :: forall row. (ChartRowAuto row) => ChartOptions -> ChartStyle -> [Column] -> [row] -> Chart
autoChartWithHeaders opts styl cols [] = buildChart opts styl cols []
autoChartWithHeaders opts styl cols (x:xs) = buildChart opts styl cols rows
  where
    rows :: [[Value]]
    rows = rowToJSON <$> (x:xs)
    rowToJSON :: row -> [Value]
    rowToJSON row = gfoldMap @ToJSON singleToRow row
    singleToRow :: forall a. ToJSON a => a -> [Value]
    singleToRow = (:[]) . toJSON
