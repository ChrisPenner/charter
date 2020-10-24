{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}
module Charts.Internal.CodeGen where

import Control.Monad.Writer
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import Text.Julius
import Text.Hamlet
import Charts.Internal.Chart
import Data.Aeson as A
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Text

toJS :: JavascriptUrl url -> Javascript
toJS f = f (\_ _ -> "unimplemented")

renderJulius :: JavascriptUrl url -> TL.Text
renderJulius = renderJavascriptUrl (\_ _ -> "unimplemented")

type ElementID = T.Text
chartToJS :: T.Text -> Chart a -> TL.Text
chartToJS targetElemID chart = renderJulius $
    [julius|
      google.charts.load('current', {'packages':['corechart']});
      google.charts.setOnLoadCallback(drawChart);

      function drawChart() {
        var data = new google.visualization.DataTable();
        #{embedJS columnHeaders}

        data.addRows(#{dataArray});

        var options = #{toJSON (options chart)};

        var chart = new google.visualization.LineChart(document.getElementById(#{targetElemID}));

        chart.draw(data, options);
      }
    |]
  where
    columnHeaders :: Javascript
    columnHeaders = foldMap columnHeaderToJS (columns chart)
    dataArray :: Value
    dataArray = toJSON (dataTable chart)

embedJS = rawJS . renderJavascript

columnHeaderToJS :: Column -> Javascript
columnHeaderToJS Column{label, typ} = toJS $ [julius|
        data.addColumn(#{toJSON typ}, #{label});|]

testColumns :: [Column]
testColumns = [Column "Year" StringColumn, Column "Attendance" NumberColumn, Column "Attendance" NumberColumn]
testChart :: Chart a
testChart = Chart testColumns testData LineChart (ChartOptions{title="My Chart"})

testData :: [[Cell]]
testData = [ ["2004",  1000,      400]
           , ["2005",  1170,      460]
           , ["2006",  660,       1120]
           , ["2007",  1030,      540]
           ]


chartToHtml :: Chart a -> TL.Text
chartToHtml chart = renderHtml [shamlet|
<html>
    <head>
        <script type="text/javascript" src="https://www.gstatic.com/charts/loader.js"></script>
        <script type="text/javascript">
            #{preEscapedToHtml chartJS}
    <body>
        <div id="chart" style="width: 900px; height: 500px"></div>
    |]
    where
      chartJS = chartToJS "chart" chart
