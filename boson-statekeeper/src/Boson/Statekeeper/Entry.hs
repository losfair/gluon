module Boson.Statekeeper.Entry (run) where

import Boson.Statekeeper.Env
import qualified Boson.Statekeeper.ProjectWorker.Manager as Manager
import Boson.Util.Exception (fromJustOrThrow)
import qualified Database.SQLite.Simple as S
import RIO
import RIO.Lens (non)
import RIO.List.Partial (head)
import RIO.Process (HasProcessContext, lookupEnvFromContext, mkDefaultProcessContext)
import RIO.Text (unpack)

data TopLevelConfig = TopLevelConfig {tlcAppConfig :: AppConfig, tlcLogLevel :: LogLevel}

run :: IO ()
run = do
  procCtx <- mkDefaultProcessContext
  runRIO procCtx run'1

run'1 :: HasProcessContext env => RIO env ()
run'1 = do
  config <- resolveConfig
  logOptions <-
    setLogUseColor True
      . setLogVerboseFormat True
      . setLogUseTime True
      . setLogUseLoc True
      . setLogMinLevel (tlcLogLevel config)
      <$> logOptionsHandle stderr False

  withLogFunc logOptions $ \logFunc -> do
    let env = GenericEnv {_geConfig = tlcAppConfig config, _geLogFunc = logFunc, _geDbOpener = liftIO $ S.open $ unpack $ tlcAppConfig config ^. dbPath}
    runRIO env run'2

run'2 :: HasEnv env => RIO env ()
run'2 = do
  checkDatabase
  Manager.run

checkDatabase :: HasEnv env => RIO env ()
checkDatabase = do
  env <- ask
  bracket (env ^. envL . geDbOpener) (liftIO . S.close) $ \db -> do
    liftIO $ S.withExclusiveTransaction db (pure ())
    (S.Only lastKnownVersion) :: S.Only Text <-
      fmap head $ liftIO $ S.query_ db "SELECT mv_last_known_version('main')"
    logInfo $ "current database version: " <> display lastKnownVersion

resolveConfig :: HasProcessContext env => RIO env TopLevelConfig
resolveConfig = do
  dbPath_ <- lookupEnvFromContext "BOSON_DB" >>= fromJustOrThrow "BOSON_DB is not set"
  flyMachineApiHostname_ <- lookupEnvFromContext "FLY_MACHINE_API_HOSTNAME" >>= fromJustOrThrow "FLY_MACHINE_API_HOSTNAME is not set"
  flyMachineApiToken_ <- lookupEnvFromContext "FLY_MACHINE_API_TOKEN" >>= fromJustOrThrow "FLY_MACHINE_API_TOKEN is not set"
  logLevelStr <- (^. non "LevelInfo") <$> lookupEnvFromContext "BOSON_LOG_LEVEL"
  logLevel_ :: LogLevel <- fromJustOrThrow ("Invalid log level: " <> logLevelStr) $ readMaybe $ unpack logLevelStr
  let appConfig =
        AppConfig
          { _dbPath = dbPath_,
            _flyConfig =
              FlyConfig
                { _flyMachineApiHostname = flyMachineApiHostname_,
                  _flyMachineApiToken = flyMachineApiToken_
                }
          }
  pure
    TopLevelConfig
      { tlcAppConfig = appConfig,
        tlcLogLevel = logLevel_
      }
