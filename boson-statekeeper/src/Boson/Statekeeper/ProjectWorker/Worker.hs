{-# LANGUAGE TemplateHaskell #-}

module Boson.Statekeeper.ProjectWorker.Worker (WorkerEnv (..), weEnv, weProjectId, runWorker) where

import Boson.Statekeeper.Env
import qualified Database.SQLite.Simple as S
import Lens.Micro.TH (makeLenses)
import RIO

data WorkerEnv = WorkerEnv
  { _weEnv :: GenericEnv,
    _weProjectId :: Text
  }

makeLenses ''WorkerEnv

instance HasEnv WorkerEnv where
  envL = weEnv

runWorker :: RIO WorkerEnv ()
runWorker = do
  env <- ask
  bracket (env ^. envL . geDbOpener) (liftIO . S.close) $ \db -> forever $ do
    runOnce db
    threadDelay $ 5 * 1000000

runOnce :: S.Connection -> RIO WorkerEnv ()
runOnce _db = do
  env <- ask
  logInfoS (logSource env) "hello"
  pure ()

logSource :: WorkerEnv -> Text
logSource env = "project:" <> env ^. weProjectId
