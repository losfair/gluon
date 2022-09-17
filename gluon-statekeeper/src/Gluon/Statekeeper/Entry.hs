module Gluon.Statekeeper.Entry (run) where

import Gluon.Statekeeper.Env
import qualified Gluon.Statekeeper.ProjectWorker.Manager as Manager
import Gluon.Util.Exception (fromJustOrThrow)
import Data.Text.Read (decimal)
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
  !dbPath_ <- lookupEnvFromContext "GLUON_DB" >>= fromJustOrThrow "GLUON_DB is not set"
  !flyMachineApiHostname_ <- lookupEnvFromContext "FLY_MACHINE_API_HOSTNAME" >>= fromJustOrThrow "FLY_MACHINE_API_HOSTNAME is not set"
  !(flyMachineApiPort_ :: Int) <-
    lookupEnvFromContext "FLY_MACHINE_API_PORT"
      & fmap (fst . (fromRight (error "invalid FLY_MACHINE_API_PORT") . decimal) . fromMaybe "4280")
  !flyMachineApiToken_ <- lookupEnvFromContext "FLY_MACHINE_API_TOKEN" >>= fromJustOrThrow "FLY_MACHINE_API_TOKEN is not set"
  !flyMachineAppName_ <- lookupEnvFromContext "FLY_MACHINE_APP_NAME" >>= fromJustOrThrow "FLY_MACHINE_APP_NAME is not set"
  logLevelStr <- (^. non "LevelInfo") <$> lookupEnvFromContext "GLUON_LOG_LEVEL"
  !(logLevel_ :: LogLevel) <- fromJustOrThrow ("Invalid log level: " <> logLevelStr) $ readMaybe $ unpack logLevelStr
  let appConfig =
        AppConfig
          { _dbPath = dbPath_,
            _flyConfig =
              FlyConfig
                { _flyMachineApiHostname = flyMachineApiHostname_,
                  _flyMachineApiPort = flyMachineApiPort_,
                  _flyMachineApiToken = flyMachineApiToken_,
                  _flyMachineAppName = flyMachineAppName_
                }
          }
  pure
    TopLevelConfig
      { tlcAppConfig = appConfig,
        tlcLogLevel = logLevel_
      }
