{-# LANGUAGE TemplateHaskell #-}

module Gluon.Statekeeper.ProjectWorker.Worker (WorkerEnv (..), weEnv, weProjectId, runWorker) where

import qualified Data.Aeson as A
import qualified Database.SQLite.Simple as S
import Gluon.Statekeeper.Env
import Gluon.Statekeeper.Fly.Machine (MachineConfig, MachineId (MachineId), miId, miPrivateIp, spinDown, spinUp, updateMachine)
import Gluon.Util.Database (retryableTxn)
import Gluon.Util.Delay (DelayConfig (DelayConfig), newDelayGenerator, runDelay)
import Gluon.Util.Exception (fromJustOrThrow)
import Gluon.Util.Time (diffTimeMillis)
import Lens.Micro.TH (makeLenses)
import RIO
import RIO.List (headMaybe)
import qualified RIO.Set
import qualified RIO.Text
import qualified RIO.Text as Text
import System.Clock (Clock (Monotonic), getTime)

data WorkerEnv = WorkerEnv
  { _weEnv :: GenericEnv,
    _weProjectId :: Text,
    _weMutationAnnotation :: Maybe Text
  }

data MutationRecord = MutationRecord
  { _mutResourceId :: Int64,
    _mutVersion :: Int,
    _mutResourceKind :: Text,
    _mutOperation :: Text,
    _mutInfo :: Text
  }

makeLenses ''WorkerEnv
makeLenses ''MutationRecord

instance HasEnv WorkerEnv where
  envL = weEnv

instance HasLogTaskSource WorkerEnv where
  logTaskSource env = case env ^. weMutationAnnotation of
    Nothing -> prefix
    Just ann -> Text.concat [prefix, "[", ann, "]"]
    where
      prefix = "project:" <> env ^. weProjectId

instance S.FromRow MutationRecord where
  fromRow = MutationRecord <$> S.field <*> S.field <*> S.field <*> S.field <*> S.field

runWorker :: RIO WorkerEnv ()
runWorker = do
  env <- ask
  delayGen <- newDelayGenerator $ DelayConfig (3 * 1000000) (7 * 1000000)
  bracket (env ^. envL . geDbOpener) (liftIO . S.close) $ \db -> forever $ do
    let task = do
          runOnce db
          runDelay delayGen
    result <- tryAny task
    case result of
      Left e -> do
        logErrorS (logTaskSource env) $ display $ Text.concat ["exception: ", Text.pack $ show e]
        runDelay delayGen
      Right _ -> pure ()

runOnce :: S.Connection -> RIO WorkerEnv ()
runOnce db = do
  env <- ask
  let projectId = env ^. weProjectId
  startTime <- liftIO $ getTime Monotonic

  (allMutations, successfulMutations) <- bracket (env ^. envL . geDbOpener) (liftIO . S.close) $ \snapshot -> do
    liftIO $ S.execute_ snapshot "BEGIN"
    mutations :: [MutationRecord] <-
      liftIO $
        S.query
          snapshot
          "SELECT resourceId, version, resourceKind, operation, info FROM Mutations WHERE projectId = ? ORDER BY resourceId ASC LIMIT 10"
          (S.Only projectId)
    successful <-
      reverse . snd
        <$> foldM
          ( \(failedResources, list) m ->
              if RIO.Set.notMember (m ^. mutResourceId) failedResources
                then
                  ( do
                      let mutationAnn =
                            Text.concat
                              [ m ^. mutResourceKind,
                                "-",
                                Text.pack $ show (m ^. mutResourceId),
                                ".v",
                                Text.pack $ show (m ^. mutVersion),
                                "-",
                                m ^. mutOperation
                              ]
                      let env' = env & weMutationAnnotation .~ Just mutationAnn
                      logInfoS (logTaskSource env') "processing mutation"
                      res <- runRIO env' $ tryAny $ handleMutation db snapshot m
                      case res of
                        Left e -> do
                          logErrorS (logTaskSource env') $
                            display $
                              Text.concat
                                [ "mutation failed: ",
                                  Text.pack $ show e
                                ]
                          pure (RIO.Set.insert (m ^. mutResourceId) failedResources, list)
                        Right _ -> pure (failedResources, m : list)
                  )
                else pure (failedResources, list)
          )
          (RIO.Set.empty, [])
          mutations
    pure (mutations, successful)
  if not (null allMutations)
    then do
      unless (null successfulMutations) $ do
        let paramList = S.SQLText projectId : map (S.SQLInteger . (^. mutResourceId)) successfulMutations
        let query = Text.concat ["DELETE FROM Mutations WHERE projectId = ? AND resourceId IN (", Text.intercalate ", " (replicate (length successfulMutations) "?"), ")"]
        retryableTxn db $ do
          liftIO $ S.execute db (S.Query query) paramList
      endTime <- liftIO $ getTime Monotonic
      let millis = diffTimeMillis endTime startTime
      logInfoS (logTaskSource env) $
        display $
          RIO.Text.concat
            [ "processed ",
              RIO.Text.pack $ show (length allMutations),
              " mutations in ",
              RIO.Text.pack $ show millis,
              " ms, ",
              RIO.Text.pack $
                show (length successfulMutations),
              " successful"
            ]
    else pure ()

handleMutation :: S.Connection -> S.Connection -> MutationRecord -> RIO WorkerEnv ()
handleMutation db snapshot mut = do
  env <- ask
  let resourceKind = mut ^. mutResourceKind
  case resourceKind of
    "Machine" -> mutateMachine db snapshot mut
    _ -> do
      logErrorS (logTaskSource env) $ display $ Text.concat ["unknown resource kind: ", resourceKind]

mutateMachine :: S.Connection -> S.Connection -> MutationRecord -> RIO WorkerEnv ()
mutateMachine db snapshot mut = do
  env <- ask
  let op = mut ^. mutOperation
  let projectId = env ^. weProjectId
  let appConfig = env ^. appConfigL
  let machineName = "m:" <> projectId <> ":" <> Text.pack (show $ mut ^. mutResourceId)

  case op of
    "delete" -> do
      spinDown (appConfig ^. flyConfig) machineName
    _ -> do
      maybeMachineRow :: Maybe (Text, Maybe Text) <-
        liftIO $
          headMaybe
            <$> S.query
              snapshot
              "SELECT config, flyId FROM Machines WHERE projectId = ? AND id = ? AND version = ?"
              (projectId, mut ^. mutResourceId, mut ^. mutVersion)
      case maybeMachineRow of
        Nothing -> do
          logInfoS (logTaskSource env) "mutation outdated, skipping"
        Just (config, flyId) -> do
          logInfoS (logTaskSource env) $ display $ Text.concat ["machine config: ", config]
          case op of
            "create" -> do
              logInfoS (logTaskSource env) $ "creating machine " <> display machineName
              case A.eitherDecodeStrict' (Text.encodeUtf8 config) of
                Left err -> do
                  logErrorS (logTaskSource env) $ display $ Text.concat ["failed to decode machine config: ", Text.pack err]
                Right (mc :: MachineConfig) -> do
                  machine <- spinUp (appConfig ^. flyConfig) machineName mc
                  retryableTxn db $ do
                    liftIO $
                      S.execute
                        db
                        "UPDATE Machines SET flyId = ?, flyPrivateIp = ? WHERE projectId = ? AND id = ?"
                        (machine ^. miId, machine ^. miPrivateIp, projectId, mut ^. mutResourceId)
                  pure ()
            "update" -> do
              logInfoS (logTaskSource env) $ "updating machine " <> display machineName
              flyId' <- fromJustOrThrow "missing fly machine id" flyId
              case A.eitherDecodeStrict' (Text.encodeUtf8 config) of
                Left err -> do
                  logErrorS (logTaskSource env) $ display $ Text.concat ["failed to decode machine config: ", Text.pack err]
                Right (mc :: MachineConfig) -> do
                  _ <- updateMachine (appConfig ^. flyConfig) (MachineId flyId') mc
                  pure ()
            _ -> do
              logErrorS (logTaskSource env) $ display $ Text.concat ["unknown operation: ", op]
      pure ()
