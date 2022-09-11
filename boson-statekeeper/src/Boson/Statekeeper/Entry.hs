module Boson.Statekeeper.Entry (run) where

import Boson.Statekeeper.Env
import Boson.Util.Exception (fromJustOrThrow)
import qualified Database.SQLite.Simple
import RIO
import RIO.Lens (non)
import RIO.Process (HasProcessContext, lookupEnvFromContext, mkDefaultProcessContext)
import RIO.Text (unpack)

data TopLevelConfig = TopLevelConfig {tlcAppConfig :: AppConfig, tlcLogLevel :: LogLevel}

data ServiceEnv = ServiceEnv {seConfig :: AppConfig, seLogFunc :: LogFunc}

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

  db <- liftIO $ Database.SQLite.Simple.open $ unpack $ tlcAppConfig config ^. dbPath

  withLogFunc logOptions $ \logFunc -> do
    let env = ServiceEnv {seConfig = tlcAppConfig config, seLogFunc = logFunc}
    runRIO env run'2

run'2 :: RIO ServiceEnv ()
run'2 = do
  env <- ask
  logInfo $ fromString $ unpack $ ("Hello World! DB path: " :: Text) <> env ^. configL . dbPath

instance {-# OVERLAPPING #-} HasEnv ServiceEnv where
  type EnvConfig ServiceEnv = AppConfig
  configL = lens seConfig (\x y -> x {seConfig = y})
  logFuncL = lens seLogFunc (\x y -> x {seLogFunc = y})

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
