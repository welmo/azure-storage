{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

module Cloud.Azure.Storage.Core
    ( -- * Account
      StorageAccount
      ( accountName
      , accountKey
      )
    , storageAccount
    , AccountName
    , AccountKey
      -- * Types
    , ContentMD5
    , Hostname
    , Resource
      -- * Request
    , withConnection
    , requestForTable
      -- * Response
    , Response (value)
    , AzureErrorMessage (lang, errorValue)
    , AzureError (code, message)
    , OdataError (odataError)
      -- * Utils
    , result
    ) where

import Control.Exception (Exception)
import Crypto.Hash (SHA256(SHA256))
import qualified Crypto.Hash as Hash
import Data.Aeson (Result(Error, Success))
import Data.Aeson.TH (deriveFromJSON, defaultOptions)
import Data.Byteable (toBytes)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base64 as Base64
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)
import Data.UnixTime (UnixTime)
import qualified Data.UnixTime as UnixTime
import Network.Http.Client (Connection, RequestBuilder, Hostname, Port, Method(..), ContentType)
import qualified Network.Http.Client as Http
import qualified OpenSSL.Session as SSL

import Cloud.Azure.Storage.Aeson (azureErrorOptions)

data AuthType = SharedKey | SharedKeyLite deriving (Show)
type AccountName = ByteString
type AccountKey = ByteString
type Signature = ByteString
type ContentMD5 = ByteString
type Resource = ByteString

data StorageAccount = StorageAccount
    { accountName :: AccountName
    , accountKey :: AccountKey
    }

storageAccount
    :: String -- ^ Account name
    -> String -- ^ Account key
    -> StorageAccount
storageAccount name key = StorageAccount
    (BC.pack name)
    (BC.pack key)

authorizationHeader :: AuthType -> AccountName -> Signature -> ByteString
authorizationHeader atype acc sig = BS.concat
    [BC.pack (show atype), " ", acc, ":", sig]

formatDate :: UnixTime -> ByteString
formatDate = UnixTime.formatUnixTimeGMT UnixTime.webDateFormat

stringToSignForTable
    :: Method
    -> Maybe ByteString -- ^ Content-MD5
    -> Maybe ContentType
    -> UnixTime
    -> AccountName
    -> ByteString -- ^ Resource
    -> ByteString
stringToSignForTable verb md5 ctype date acc res = BS.intercalate "\n"
    [ BC.pack $ show verb
    , fromMaybe "" md5
    , fromMaybe "" ctype
    , formatDate date
    , BS.concat ["/", acc, res]
    ]

fromEither :: a -> Either b a -> a
fromEither _ (Right a) = a
fromEither a _         = a

signature :: AccountKey -> ByteString -> ByteString
signature key
    = Base64.encode
    . toBytes
    . Hash.hmacAlg SHA256 (fromEither key $ Base64.decode key)

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust Nothing  _ = return ()
whenJust (Just a) f = f a

requestForTable
    :: AccountName
    -> AccountKey
    -> Hostname
    -> Method
    -> Maybe ContentMD5
    -> Maybe ContentType
    -> UnixTime
    -> Resource
    -> RequestBuilder ()
requestForTable acc key host verb md5 ctype date res = do
    Http.setHostname host 443
    Http.http verb res
    Http.setAccept "application/json;odata=nometadata"
    Http.setHeader "Date" $ formatDate date
    Http.setHeader "DataServiceVersion" "3.0"
    Http.setHeader "x-ms-version" "2014-02-14"
    Http.setHeader "Accept-Encoding" "UTF-8"
    whenJust md5 $ Http.setHeader "Content-MD5"
    whenJust ctype $ Http.setContentType
    Http.setHeader "Authorization"
        $ authorizationHeader SharedKey acc
        $ signature key
        $ stringToSignForTable verb md5 ctype date acc res

withConnection :: Hostname -> Port -> (Connection -> IO a) -> IO a
withConnection host port f = do
    sslContext <- SSL.context
    Http.withConnection (Http.openConnectionSSL sslContext host port) f

result :: (String -> r) -> (a -> r) -> Result a -> r
result f _ (Error msg) = f msg
result _ f (Success a) = f a

newtype Response a = Response { value :: [a] }

deriveFromJSON defaultOptions ''Response

data AzureErrorMessage = AzureErrorMessage
    { lang :: String
    , errorValue :: String
    }
  deriving (Show)

deriveFromJSON azureErrorOptions ''AzureErrorMessage

data AzureError = AzureError
    { code :: String
    , message :: AzureErrorMessage
    }
  deriving (Show, Typeable)

deriveFromJSON azureErrorOptions ''AzureError

instance Exception AzureError

newtype OdataError = OdataError { odataError :: AzureError }

deriveFromJSON azureErrorOptions ''OdataError
