{-# LANGUAGE TemplateHaskell, QuasiQuotes, FlexibleInstances, UndecidableInstances #-}

module Cloud.Azure.Storage.Table
    ( TableName(..)
    , queryTables
    , queryEntity
    , TableEntity
    , deriveTableEntity
    , insertEntity
    ) where

import Control.Applicative
import Control.Arrow (first)
import Control.Monad (unless)
import Data.Aeson (FromJSON, ToJSON, Value, Result(Success, Error))
import qualified Data.Aeson as Aeson
import Data.Aeson.TH (deriveJSON, deriveFromJSON, defaultOptions)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.Monoid ((<>))
import Data.String (IsString(fromString))
import qualified Data.UnixTime as UnixTime
import Language.Haskell.TH
import Network.Http.Client (Method(..))
import qualified Network.Http.Client as Http
import qualified System.IO.Streams as Streams
import qualified System.IO.Streams.Attoparsec as SA

import Cloud.Azure.Storage.Aeson (azureOptions)
import Cloud.Azure.Storage.Core (Resource, StorageAccount)
import qualified Cloud.Azure.Storage.Core as Core

data TableOperation a = TableOperation
    { method :: Method
    , resource :: Resource
    , param :: Maybe a
    }

result :: (String -> r) -> (a -> r) -> Result a -> r
result f _ (Error msg) = f msg
result _ f (Success a) = f a

newtype Response a = Response { value :: [a] }

deriveFromJSON defaultOptions ''Response

runStorageTable1 :: ToJSON a
    => StorageAccount -> TableOperation a -> IO Value
runStorageTable1 acc op = do
    Core.withConnection host 443 $ \conn -> do
        time <- UnixTime.getUnixTime
        req <- Http.buildRequest $ Core.requestForTable
                (Core.accountName acc)
                (Core.accountKey acc)
                host
                (method op)
                Nothing
                (Just "application/json")
                time
                (resource op)

        Http.sendRequest conn req =<< body (param op)
        Http.receiveResponse conn $ \_res i ->
            SA.parseFromStream Aeson.json i
  where
    host = Core.accountName acc <> ".table.core.windows.net"
    body = maybe (return Http.emptyBody) $ \p ->
        Http.inputStreamBody <$> Streams.fromLazyByteString (Aeson.encode p)

runStorageTable :: (FromJSON a, ToJSON a)
    => StorageAccount
    -> TableOperation a
    -> IO [a]
runStorageTable acc op = runStorageTable1 acc op >>=
    result fail (return . value) . Aeson.fromJSON

runStorageTable' :: (FromJSON a, ToJSON a)
    => StorageAccount
    -> TableOperation a
    -> IO a
runStorageTable' acc op = runStorageTable1 acc op >>=
    result fail return . Aeson.fromJSON

newtype TableName = TableName { tableName :: String }
  deriving (Show)

instance IsString TableName where
    fromString = TableName

tableNameBS :: TableName -> ByteString
tableNameBS = BC.pack . tableName

deriveJSON azureOptions ''TableName

queryTables :: StorageAccount -> IO [TableName]
queryTables acc = runStorageTable acc $ TableOperation
    GET
    "/Tables"
    Nothing

class (FromJSON a, ToJSON a) => TableEntity a

instance (FromJSON a, ToJSON a) => TableEntity a

fNames :: Con -> [(Name, Type)]
fNames (RecC _ vsts) = map (\(a, _, b) -> (a, b)) vsts
fNames _             = []

requiredFields :: [String]
requiredFields = ["partitionKey", "rowKey", "timestamp"]

checkRequiredFields :: [(Name, Type)] -> Bool
checkRequiredFields names = and (map f requiredFields)
  where
    ns = map (first nameBase) names
    f fname = maybe False isString $ lookup fname ns
    isString (ConT n) = nameBase n == "String"
    isString _        = False

deriveTableEntity :: Name -> DecsQ
deriveTableEntity name = do
    TyConI (DataD _ _ _ cs _) <- reify name
    unless (and $ map (checkRequiredFields . fNames) cs)
        $ fail $ show name
            ++ " must have the following String fields: "
            ++ show requiredFields
    deriveJSON azureOptions name

queryEntity :: TableEntity a => StorageAccount -> TableName -> IO [a]
queryEntity acc table = runStorageTable acc $ TableOperation
    GET
    ("/" <> tableNameBS table)
    Nothing

insertEntity :: TableEntity a => StorageAccount -> TableName -> a -> IO a
insertEntity acc table a = runStorageTable' acc $ TableOperation
    POST
    ("/" <> tableNameBS table)
    (Just a)
