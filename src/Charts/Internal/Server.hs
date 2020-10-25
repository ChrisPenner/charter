{-# LANGUAGE OverloadedStrings #-}
module Charts.Internal.Server where


import Charts.Internal.Chart
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types.Status
import Data.Aeson

chartApp :: Chart a -> Application
chartApp chart req handler 
      | pathInfo req == ["data"] = do
          handler (responseLBS ok200 [("Content-Type", "application/json")] (encode chart))
      | otherwise = do
          handler (responseFile ok200 mempty "./templates/index.html" Nothing)

serveChart :: Port -> Chart a -> IO ()
serveChart port chart = run port (chartApp chart)
