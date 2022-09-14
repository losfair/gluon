{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Boson.Statekeeper.Env
  ( HasEnv (..),
    HasConfig (..),
    AppConfig (..),
    FlyConfig (..),
    GenericEnv (..),
    HasLogTaskSource (..),
    geConfig,
    geLogFunc,
    geDbOpener,
    dbPath,
    flyConfig,
    flyMachineApiHostname,
    flyMachineApiToken,
  )
where

import qualified Database.SQLite.Simple as S
import Lens.Micro.TH
import RIO

data GenericEnv = GenericEnv
  { _geConfig :: AppConfig,
    _geLogFunc :: LogFunc,
    _geDbOpener :: forall env. RIO env S.Connection
  }

data AppConfig = AppConfig
  { _dbPath :: Text,
    _flyConfig :: FlyConfig
  }

data FlyConfig = FlyConfig
  { _flyMachineApiHostname :: Text,
    _flyMachineApiToken :: Text
  }

makeLenses ''GenericEnv
makeLenses ''AppConfig
makeLenses ''FlyConfig

class HasLogFunc a => HasEnv a where
  envL :: Lens' a GenericEnv

class HasConfig a where
  appConfigL :: Lens' a AppConfig

class HasLogTaskSource a where
  logTaskSource :: a -> Text

instance HasEnv GenericEnv where
  envL = id

instance HasEnv a => HasConfig a where
  appConfigL = envL . geConfig . appConfigL

instance {-# OVERLAPPING #-} HasConfig AppConfig where
  appConfigL = id

instance HasEnv a => HasLogFunc a where
  logFuncL = envL . geLogFunc
