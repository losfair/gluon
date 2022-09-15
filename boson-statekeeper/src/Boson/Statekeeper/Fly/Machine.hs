{-# LANGUAGE TemplateHaskell #-}

module Boson.Statekeeper.Fly.Machine
  ( MachineConfig (..),
    MachineInfo (..),
    MachineId (..),
    spinUp,
    spinDown,
    miId,
  )
where

import Boson.Statekeeper.Env
  ( FlyConfig,
    HasLogTaskSource (logTaskSource),
    flyMachineApiHostname,
    flyMachineApiPort,
    flyMachineApiToken,
    flyMachineAppName,
  )
import Boson.Util.Http (isStatusCodeException)
import Data.Aeson
import Data.List (find)
import Lens.Micro.TH (makeLenses)
import qualified Network.HTTP.Client as L
import Network.HTTP.Req
import Network.HTTP.Types (Status (statusCode))
import RIO

data MachineConfig = MachineConfig
  { _mcImage :: Text,
    _mcEnv :: Maybe (RIO.Map Text Text),
    _mcGuest :: Maybe GuestAllocation
  }
  deriving (Generic)

data GuestAllocation = GuestAllocation
  { _gaCpus :: Int,
    _gaMemoryMB :: Int
  }
  deriving (Generic)

data CreateMachinePayload = CreateMachinePayload
  { _cmpName :: Text,
    _cmpConfig :: MachineConfig
  }
  deriving (Generic)

data MachineInfo = MachineInfo
  { _miId :: Text,
    _miName :: Text,
    _miState :: Text,
    _miRegion :: Text,
    _miInstanceId :: Text,
    _miPrivateIp :: Text,
    _miConfig :: Value,
    _miImageRef :: Value
  }
  deriving (Generic, Show)

newtype MachineId = MachineId Text

makeLenses ''MachineConfig
makeLenses ''GuestAllocation
makeLenses ''CreateMachinePayload
makeLenses ''MachineInfo

mcJsonCodec :: String -> String
mcJsonCodec = camelTo2 '_' . drop 3

gaJsonCodec :: String -> String
gaJsonCodec = camelTo2 '_' . drop 3

cmpJsonCodec :: String -> String
cmpJsonCodec = camelTo2 '_' . drop 4

miJsonCodec :: String -> String
miJsonCodec = camelTo2 '_' . drop 3

instance FromJSON MachineConfig where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = mcJsonCodec}

instance ToJSON MachineConfig where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = mcJsonCodec}

instance FromJSON GuestAllocation where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = gaJsonCodec}

instance ToJSON GuestAllocation where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = gaJsonCodec}

instance FromJSON CreateMachinePayload where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = cmpJsonCodec}

instance ToJSON CreateMachinePayload where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = cmpJsonCodec}

instance FromJSON MachineInfo where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = miJsonCodec}

instance ToJSON MachineInfo where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = miJsonCodec}

spinUp :: (HasLogFunc env, HasLogTaskSource env) => FlyConfig -> Text -> MachineConfig -> RIO env MachineInfo
spinUp config name ms = do
  env <- ask
  let apiHostname = config ^. flyMachineApiHostname
  let appName = config ^. flyMachineAppName
  -- /v1/apps/user-functions/machines
  res :: Either HttpException (JsonResponse MachineInfo) <-
    try $
      runReq defaultHttpConfig $
        req POST (http apiHostname /: "v1" /: "apps" /: appName /: "machines") (ReqBodyJson $ CreateMachinePayload name ms) jsonResponse $
          reqScheme config
  case res of
    Left exc -> do
      case isStatusCodeException exc of
        Just (httpRes, body) -> do
          let code = statusCode $ L.responseStatus httpRes
          if code == 422
            then do
              logWarnS (logTaskSource env) $ "got status 422: " <> displayShow body
              existingMachine <- getMachineByName config name
              case existingMachine of
                Just machine -> do
                  logInfoS (logTaskSource env) "found existing machine"
                  pure machine
                Nothing -> do
                  logErrorS (logTaskSource env) "could not find existing machine"
                  throwIO exc
            else throwIO exc
        Nothing -> throwIO exc
    Right httpRes -> do
      -- let status = responseStatusCode res
      let machine :: MachineInfo = responseBody httpRes
      pure machine

spinDown :: FlyConfig -> Text -> RIO env ()
spinDown config name = do
  let apiHostname = config ^. flyMachineApiHostname
  let appName = config ^. flyMachineAppName
  maybeMachine <- getMachineByName config name
  case maybeMachine of
    Nothing -> pure ()
    Just machine -> do
      let machineId = machine ^. miId
      stopMachine config (MachineId machineId)
      res :: Either HttpException (JsonResponse Value) <-
        try $
          runReq defaultHttpConfig $
            req DELETE (http apiHostname /: "v1" /: "apps" /: appName /: "machines" /: machineId) NoReqBody jsonResponse $
              reqScheme config
      case res of
        Left exc -> do
          case isStatusCodeException exc of
            Just (httpRes, _) ->
              if statusCode (L.responseStatus httpRes) == 404
                then pure ()
                else throwIO exc
            Nothing -> throwIO exc
        Right _ -> do
          pure ()

stopMachine :: FlyConfig -> MachineId -> RIO env ()
stopMachine config (MachineId machineId) = do
  let apiHostname = config ^. flyMachineApiHostname
  let appName = config ^. flyMachineAppName
  res :: Either HttpException (JsonResponse Value) <-
    try $
      runReq defaultHttpConfig $
        req POST (http apiHostname /: "v1" /: "apps" /: appName /: "machines" /: machineId /: "stop") NoReqBody jsonResponse $
          reqScheme config
  case res of
    Left exc -> do
      case isStatusCodeException exc of
        Just (httpRes, _) ->
          if statusCode (L.responseStatus httpRes) == 404
            then pure ()
            else throwIO exc
        Nothing -> throwIO exc
    Right _ -> do
      pure ()

getMachineByName :: FlyConfig -> Text -> RIO env (Maybe MachineInfo)
getMachineByName config name = do
  list <- listMachines config
  pure $ find (\x -> x ^. miName == name) list

listMachines :: FlyConfig -> RIO env [MachineInfo]
listMachines config = do
  let apiHostname = config ^. flyMachineApiHostname
  let appName = config ^. flyMachineAppName
  res :: JsonResponse [MachineInfo] <-
    runReq defaultHttpConfig $
      req GET (http apiHostname /: "v1" /: "apps" /: appName /: "machines") NoReqBody jsonResponse $
        reqScheme config
  pure $ responseBody res

reqScheme :: FlyConfig -> Option scheme
reqScheme config =
  port (config ^. flyMachineApiPort) <> header "authorization" (encodeUtf8 ("Bearer " <> config ^. flyMachineApiToken))
