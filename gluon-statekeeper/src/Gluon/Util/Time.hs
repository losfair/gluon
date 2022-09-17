module Gluon.Util.Time (diffTimeMillis) where

import RIO
import System.Clock (TimeSpec, diffTimeSpec, toNanoSecs)

diffTimeMillis :: TimeSpec -> TimeSpec -> Double
diffTimeMillis t1 t2 = millis
  where
    micros = toNanoSecs (diffTimeSpec t1 t2) `div` 1000
    millis = (fromInteger micros :: Double) / 1000
