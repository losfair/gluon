module Gluon.Util.Delay (DelayGenerator, DelayConfig (..), newDelayGenerator, runDelay) where

import Crypto.Random (ChaChaDRG)
import qualified Crypto.Random
import qualified Data.Binary.Get as B
import GHC.IO.Exception (userError)
import RIO
import RIO.ByteString.Lazy (fromStrict)

data DelayGenerator = DelayGenerator (SomeRef ChaChaDRG) DelayConfig

data DelayConfig = DelayConfig
  { minDelayMicros :: Int,
    maxDelayMicros :: Int
  }

newDelayGenerator :: MonadIO m => DelayConfig -> m DelayGenerator
newDelayGenerator config = do
  drg <- newSomeRef =<< liftIO Crypto.Random.drgNew
  return $ DelayGenerator drg config

runDelay :: (MonadIO m, MonadThrow m) => DelayGenerator -> m ()
runDelay (DelayGenerator drg config) = do
  let max_ = maxDelayMicros config
  let min_ = minDelayMicros config
  when (max_ < min_) $ do
    throwM $ userError "maxDelayMicros must not be smaller than minDelayMicros"
  delayMicros <-
    if max_ > min_
      then do
        drg' <- readSomeRef drg
        let (randBytes, drg'') = Crypto.Random.randomBytesGenerate 8 drg'
        writeSomeRef drg drg''
        let word = B.runGet B.getWord64be (fromStrict randBytes)
        let micros = fromIntegral (word `mod` (fromIntegral (max_ - min_) :: Word64)) + min_
        return micros
      else do
        return min_
  liftIO $ threadDelay delayMicros
