module Avers.Metrics where


import           Control.Monad.IO.Class
import           Control.Monad.State
import           Control.Exception.Base

import           System.Clock

import           Avers.Types
import           Avers.Metrics.Measurements



measureDuration :: Measurement -> Avers a -> Avers a
measureDuration m a = do
    start <- liftIO $ getTime Monotonic
    ret   <- a >>= liftIO . evaluate
    end   <- liftIO $ getTime Monotonic

    reportMeasurement m
        (fromIntegral (toNanoSecs (diffTimeSpec start end)) / 1000000000)

    return ret


reportMeasurement :: Measurement -> Double -> Avers ()
reportMeasurement m value = do
    conf <- gets hConfig
    liftIO $ emitMeasurement conf m value
