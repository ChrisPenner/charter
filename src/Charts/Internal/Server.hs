{-# LANGUAGE OverloadedStrings #-}
module Charts.Internal.Server where


import Charts.Internal.Chart
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types.Status
import Data.Aeson
import Paths_charter

chartApp :: Chart a -> FilePath -> Application
chartApp chart indexFile req handler 
      | pathInfo req == ["data"] = do
          handler (responseLBS ok200 [("Content-Type", "application/json")] (encode chart))
      | otherwise = do
          handler (responseFile ok200 mempty indexFile Nothing)

serveChart :: Port -> Chart a -> IO ()
serveChart port chart = do
    indexHtml <- getDataFileName "templates/index.html"
    run port (chartApp chart indexHtml)
