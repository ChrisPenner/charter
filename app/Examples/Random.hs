{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Examples.Random where

import Charts
import Control.Monad.Random
import Data.Traversable
import Data.Aeson
import Control.Concurrent

randomChart :: IO Chart
randomChart = do
    rows <- for [1..10] $ \i -> do
                a <- randomRIO (0, 1)
                b <- randomRIO (0, 1)
                return [toJSON @Int i, toJSON @Float a, toJSON @Double b]
    return $ buildChart defaultChartOptions ScatterChart [NumberColumn "Iteration", NumberColumn "Value A", NumberColumn "Value B"] rows

buildRandomChart :: (Chart -> IO ()) -> IO ()
buildRandomChart sendChart = forever $ do
    randomChart >>= sendChart
    threadDelay 2000000

serveUpdatingRandomChart :: IO ()
serveUpdatingRandomChart = serveDynamicChart 8080 buildRandomChart
