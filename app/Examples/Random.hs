{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Examples.Random where

import Charts
import Control.Monad.Random
import Data.Traversable
import Data.Aeson

randomChart :: Chart
randomChart = buildChart
  defaultChartOptions
  [NumberColumn "Iteration", NumberColumn "Value A", NumberColumn "Value B"]
  $ flip evalRand (mkStdGen 3) $ for [1..100] $ \i -> do
                        a <- liftRand (randomR (0, 1))
                        b <- liftRand (randomR (0, 1))
                        return [toJSON @Int i, toJSON @Float a, toJSON @Double b]
