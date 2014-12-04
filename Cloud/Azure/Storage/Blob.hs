module Cloud.Azure.Storage.Blob
    ( getBlob
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.Monoid ((<>))
import qualified Data.UnixTime as UnixTime
import Network.Http.Client (Method(..))
import qualified Network.Http.Client as Http
import System.IO.Streams (InputStream)
import qualified System.IO.Streams as Streams

import Cloud.Azure.Storage.Core (StorageAccount)
import qualified Cloud.Azure.Storage.Core as Core

type Container = ByteString
type BlobName = ByteString

getBlob
    :: StorageAccount
    -> Container
    -> BlobName
    -> (InputStream ByteString -> IO a)
    -> IO a
getBlob acc container blob f = Core.withConnection host 443 $ \conn -> do
    time <- UnixTime.getUnixTime
    req <- Http.buildRequest $ Core.request
            (Core.accountName acc)
            (Core.accountKey acc)
            host
            GET
            Nothing
            (Just "application/xml")
            time
            (BC.concat ["/", container, "/", blob])
    print req
    Http.sendRequest conn req Http.emptyBody
    Http.receiveResponse conn $ \res is ->
        if Http.getStatusCode res == 200
            then f is
            else Streams.read is >>= print >> fail "unknown error"
  where
    host = Core.accountName acc <> ".blob.core.windows.net"
