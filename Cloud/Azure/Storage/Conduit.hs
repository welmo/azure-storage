module Cloud.Azure.Storage.Conduit
    ( streamToConduit
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Conduit (ConduitM)
import qualified Data.Conduit as Conduit
import System.IO.Streams (InputStream)
import qualified System.IO.Streams as Streams

streamToConduit :: MonadIO m => InputStream o -> ConduitM i o m ()
streamToConduit is = do
    mo <- liftIO $ Streams.read is
    case mo of
        Nothing -> return ()
        Just o  -> do
            Conduit.yield o
            streamToConduit is
