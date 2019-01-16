{-| Integration in "MonadIO". -}
module Potoki.Conduit.MonadIO
(
  consumeConduit,
)
where

import Potoki.Conduit.Prelude
import Conduit
import qualified Potoki.Core.Produce as Produce
import qualified Potoki.Core.Transform as Transform
import qualified Potoki.Core.Consume as Consume
import qualified Potoki.Core.Fetch as Fetch
import qualified Potoki.Core.IO as IO


{-| Given a Conduit source over a "MonadIO" monad and a Potoki consumer
    execute the whole pipeline in the base monad of the source. -}
consumeConduit :: MonadIO m => ConduitT () o m () -> Consume.Consume o r -> m r
consumeConduit conduit consume = do
  elementChan <- liftIO $ newTBMChanIO 100
  resultVar <- liftIO $ newEmptyMVar
  liftIO $ fork $ do
    result <- IO.produceAndConsume (Produce.tbmChan elementChan) consume
    putMVar resultVar result
  feedConduitToTBMChan conduit elementChan
  liftIO $ takeMVar resultVar

feedConduitToMVar :: MonadIO m => ConduitT () o m () -> MVar (Maybe o) -> m ()
feedConduitToMVar conduit mvar = do
  runConduit $ conduit .| mapM_C (liftIO . putMVar mvar . Just)
  liftIO (putMVar mvar Nothing)

feedConduitToTBMChan :: MonadIO m => ConduitT () o m () -> TBMChan o -> m ()
feedConduitToTBMChan conduit chan = do
  runConduit $ conduit .| mapM_C (liftIO . atomically . writeTBMChan chan)
  liftIO $ atomically $ closeTBMChan chan
