module Boson.Util.Database (retryableTxn) where

import Boson.Statekeeper.Env (HasLogTaskSource (logTaskSource))
import qualified Database.SQLite.Simple as S
import RIO

retryableTxn :: (HasLogFunc env, HasLogTaskSource env) => S.Connection -> RIO env a -> RIO env a
retryableTxn conn action = do
  env <- ask
  liftIO $ S.execute_ conn "BEGIN IMMEDIATE"
  ret <- onException action $ do
    liftIO $ S.execute_ conn "ROLLBACK"
  commitRes <- tryAny $ liftIO $ S.execute_ conn "COMMIT"
  case commitRes of
    Left e -> case fromException e :: Maybe S.SQLError of
      Just sqlE -> do
        let code = S.sqlError sqlE
        if code == S.ErrorBusy
          then do
            liftIO $ S.execute_ conn "ROLLBACK"
            logWarnS (logTaskSource env) "retrying transaction"
            retryableTxn conn action
          else throwIO e
      Nothing -> throwIO e
    Right _ -> pure ret
