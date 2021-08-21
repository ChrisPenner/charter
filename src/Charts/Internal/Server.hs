{-# LANGUAGE OverloadedStrings #-}
module Charts.Internal.Server where


import Charts.Internal.Chart
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types.Status
import Data.Aeson
import Paths_charter
import Control.Concurrent.Async
import System.Process
import Control.Monad
import Control.Concurrent.MVar

-- | Serve a chart, updating along with the MVar
chartApp :: MVar Chart -> FilePath -> Application
chartApp chartVar indexFile req handler
      | pathInfo req == ["data"] = do
          chart <- readMVar chartVar
          handler (responseLBS ok200 [("Content-Type", "application/json")] (encode chart))
      | otherwise = do
          handler (responseFile ok200 mempty indexFile Nothing)

-- | Serve a single static chart on the given port
serveChart :: Port ->  Chart -> IO ()
serveChart port chart = do
    serveDynamicChart port (\handler -> handler (chart{dynamic=False}))

-- | Serve a chart on the given port.
-- The application can update the chart using the given handler.
serveDynamicChart :: Port -> ((Chart -> IO ()) -> IO ()) -> IO ()
serveDynamicChart port handler = do
    indexHtml <- getDataFileName "templates/index.html"
    chartVar <- newEmptyMVar
    let runServer = run port (chartApp chartVar indexHtml)
    let runHandler = handler (updateChart chartVar)
    withAsync (concurrently_ runServer runHandler) $ \procHandle -> do
        putStrLn $ "Serving chart on port: " <> show port
        void $ spawnProcess "open" [("http://localhost:" <> show port)]
        wait procHandle
  where
    updateChart :: MVar Chart -> Chart -> IO ()
    updateChart var c = void $ do
      isEmpty <- isEmptyMVar var
      if isEmpty then putMVar var c
                 else void $ swapMVar var c

