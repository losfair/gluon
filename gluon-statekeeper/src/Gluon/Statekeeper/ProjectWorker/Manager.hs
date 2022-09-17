{-# LANGUAGE TemplateHaskell #-}

module Gluon.Statekeeper.ProjectWorker.Manager (run) where

import Gluon.Statekeeper.Env
import Gluon.Statekeeper.ProjectWorker.Worker
import Gluon.Util.Time (diffTimeMillis)
import qualified Database.SQLite.Simple as S
import Lens.Micro.TH (makeLenses)
import RIO
import qualified RIO.Map
import qualified RIO.Set
import RIO.State (MonadState (get, put))
import qualified RIO.Text
import System.Clock (Clock (Monotonic), getTime)

data LocalEnv = LocalEnv
  { _leState :: SomeRef LocalState,
    _leEnv :: GenericEnv
  }

newtype LocalState = LocalState
  { _lsWorkers :: Map Text Worker
  }

newtype Worker = Worker
  { _wTask :: Async ()
  }

makeLenses ''LocalEnv
makeLenses ''LocalState
makeLenses ''Worker

instance HasEnv LocalEnv where
  envL = leEnv

instance HasStateRef LocalState LocalEnv where
  stateRefL = leState

run :: HasEnv env => RIO env ()
run = do
  outerEnv <- (^. envL) <$> ask
  initialState <-
    newSomeRef $
      LocalState
        { _lsWorkers = mempty
        }
  let localEnv =
        LocalEnv
          { _leEnv = outerEnv,
            _leState = initialState
          }
  runRIO localEnv $
    bracket (outerEnv ^. envL . geDbOpener) (liftIO . S.close) $ \db -> forever $ do
      runOnce db
      threadDelay $ 5 * 1000000

runOnce :: S.Connection -> RIO LocalEnv ()
runOnce db = do
  startTime <- liftIO $ getTime Monotonic
  projectList :: [S.Only Text] <- liftIO $ S.query_ db "SELECT id FROM Projects UNION SELECT DISTINCT projectId as id from Mutations"
  outerEnv <- (^. leEnv) <$> ask
  st <- get
  let oldProjectIds = RIO.Map.keysSet $ st ^. lsWorkers
  let newProjectIds = RIO.Set.fromList $ map (\(S.Only x) -> x) projectList

  let removedIds = RIO.Set.difference oldProjectIds newProjectIds
  let addedIds = RIO.Set.difference newProjectIds oldProjectIds

  workerList_afterRemove <-
    filterM
      ( \(projectId, worker) -> do
          if RIO.Set.member projectId removedIds
            then do
              logInfoS logSource $ display $ RIO.Text.concat ["stopping worker ", projectId]
              liftIO $ cancel $ worker ^. wTask
              pure False
            else pure True
      )
      (RIO.Map.toList $ st ^. lsWorkers)

  addedWorkers <-
    mapM
      ( \projectId -> do
          logInfoS logSource $ display $ RIO.Text.concat ["starting worker ", projectId]
          let workerEnv = WorkerEnv {_weEnv = outerEnv, _weProjectId = projectId, _weMutationAnnotation = Nothing}
          task <- runRIO workerEnv $
            async $ do
              output <- tryAny runWorker
              case output of
                Left e -> do
                  logErrorS logSource $ display $ RIO.Text.concat ["worker ", projectId, " crashed: ", RIO.Text.pack $ show e]
                Right () -> do
                  return ()
          pure (projectId, Worker {_wTask = task})
      )
      (RIO.Set.toList addedIds)

  let newWorkerMap = RIO.Map.fromList $ workerList_afterRemove ++ addedWorkers

  put $
    st & lsWorkers
      .~ newWorkerMap

  endTime <- liftIO $ getTime Monotonic
  if not (null removedIds && null addedIds)
    then do
      let millis = diffTimeMillis endTime startTime
      logInfoS logSource $ display $ RIO.Text.concat ["finished handling project changes in ", RIO.Text.pack $ show millis, " ms"]
    else pure ()

  pure ()

logSource :: Text
logSource = "manager"
