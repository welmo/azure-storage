module Cloud.Azure.Storage.Blob.Types
    (
    ) where

import Data.Text (Text)
import Data.UnixTime (UnixTime)

data BlobType = BlockBlob | PageBlob
  deriving (Show, Eq)

data LeaseStatus = Locked | Unlocked
  deriving (Show, Eq)

data LeaseState = Available | Leased | Expired | Breaking | Broken
  deriving (Show, Eq)

data CopyStatus = Pending | Success | Aborted | Failed
  deriving (Show, Eq)

data Properties = Properties
    { lastModified :: UnixTime
    , etag :: Text
    , contentLength :: Int
    , contentType :: Text
    , contentEncoding :: Maybe Text
    , contentLanguage :: Maybe Text
    , contentMD5 :: Maybe Text
    , cacheControl :: Maybe Text
    , blobSequenceNumber :: Text
    , blobType :: BlobType
    , leaseStatus :: LeaseStatus
    , leaseState :: LeaseState
    , leaseDuration :: LeaseDuration
    , copyId :: Text
    , copyStatus :: CopyStatus
    , copySource :: Text
    , copyProgress :: Text
    , copyCompletionTime :: UnixTime
    , copyStatusDescription :: Text
    }
  deriving (Show, Eq)
