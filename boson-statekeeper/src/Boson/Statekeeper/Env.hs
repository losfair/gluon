{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Boson.Statekeeper.Env
  ( HasEnv (..),
    HasConfig (..),
    AppConfig (..),
    FlyConfig (..),
    dbPath,
    flyConfig,
    flyMachineApiHostname,
    flyMachineApiToken,
  )
where

import Lens.Micro.TH
import RIO

class HasEnv a where
  type EnvConfig a
  configL :: Lens' a (EnvConfig a)
  logFuncL :: Lens' a LogFunc

instance (HasEnv a, HasConfig (EnvConfig a)) => HasConfig a where
  appConfigL = configL . appConfigL

instance HasEnv a => HasLogFunc a where
  logFuncL = Boson.Statekeeper.Env.logFuncL

class HasConfig a where
  appConfigL :: Lens' a AppConfig

data AppConfig = AppConfig
  { _dbPath :: Text,
    _flyConfig :: FlyConfig
  }

instance HasConfig AppConfig where
  appConfigL = id

data FlyConfig = FlyConfig
  { _flyMachineApiHostname :: Text,
    _flyMachineApiToken :: Text
  }

makeLenses ''AppConfig
makeLenses ''FlyConfig
