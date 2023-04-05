{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}

module State where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Reader
import Database.SQLite.Simple
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Debug.Trace

newtype HiobeM a = HiobeM
    { runHiobeM :: ReaderT (TVar HiobeState) IO a
    }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader (TVar HiobeState))

hiobeM :: MonadTrans t => HiobeM a -> t HiobeM a
hiobeM = lift

gets :: (HiobeState -> a) -> HiobeM a
gets g = asks readTVarIO >>= liftIO >>= return . g

modify :: (HiobeState -> HiobeState) -> HiobeM ()
modify g = ask >>= liftIO . atomically . flip modifyTVar g

modify' :: (HiobeState -> HiobeState) -> HiobeM ()
modify' g = ask >>= liftIO . atomically . flip modifyTVar' g

data HiobeState = HiobeState
    { dbConn          :: MVar Connection
    , reqCount        :: Map Text Integer
    , respCount       :: Integer
    , langEngagements :: Map Text Integer
    , enableTraces    :: Bool
    }

initState :: Bool -> MVar Connection -> HiobeState
initState traces dbConn =
    HiobeState
      { dbConn          = dbConn
      , langEngagements = Map.empty
      , reqCount        = Map.empty
      , respCount       = 0
      , enableTraces    = traces
      }

putLang :: Text -> HiobeM ()
putLang l =
    modify' $ \HiobeState{..} ->
      HiobeState dbConn reqCount respCount (Map.insertWith (+) l 1 langEngagements) enableTraces

putReq :: Text -> HiobeM ()
putReq p =
    modify' $ \HiobeState{..} ->
      HiobeState dbConn (Map.insertWith (+) p 1 reqCount) respCount langEngagements enableTraces

putResp :: HiobeM ()
putResp =
    modify' $ \HiobeState{..} ->
      let newCount =
            if enableTraces && respCount `mod` 100 == 0 then
              traceMarker (show respCount ++ " survey responses") $ respCount + 1
            else
              respCount + 1
      in seq newCount $ HiobeState dbConn reqCount newCount langEngagements enableTraces
