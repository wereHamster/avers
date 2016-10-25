{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}


module Avers.API.Types where

import GHC.Generics

import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict, fromStrict)
import Data.Text   (Text)
import Data.Aeson  as A
import Data.Vector as V
import Data.Time
import Avers
import Servant.API (MimeRender(..), MimeUnrender(..), OctetStream)




--------------------------------------------------------------------------------
-- CreateObject

data CreateObjectBody = CreateObjectBody
    { cobType :: !Text
    , cobContent :: !Value
    } deriving (Generic)

instance FromJSON CreateObjectBody where
    parseJSON (A.Object o) = CreateObjectBody <$> o .: "type" <*> o .: "content"
    parseJSON _            = fail "CreateObjectBody"


data CreateObjectResponse = CreateObjectResponse
    { corId :: !ObjId
    , corType :: !Text
    , corContent :: !Value
    } deriving (Generic)

instance ToJSON CreateObjectResponse where
    toJSON x = object
        [ "id" .= corId x
        , "type" .= corType x
        , "content" .= corContent x
        ]



--------------------------------------------------------------------------------
-- LookupObject

data LookupObjectResponse = LookupObjectResponse
    { lorId :: !ObjId
    , lorType :: !Text
    , lorCreatedAt :: !UTCTime
    , lorCreatedBy :: !ObjId
    , lorRevisionId :: !RevId
    , lorContent :: !Value
    } deriving (Generic)

instance ToJSON LookupObjectResponse where
    toJSON x = object
        [ "id" .= lorId x
        , "type" .= lorType x
        , "createdAt" .= lorCreatedAt x
        , "createdBy" .= lorCreatedBy x
        , "revisionId" .= lorRevisionId x
        , "content" .= lorContent x
        ]



--------------------------------------------------------------------------------
-- PatchObject

data PatchObjectBody = PatchObjectBody
    { pobRevisionId :: !RevId
      -- ^ The 'RevId' against which the client created the operations. This may
      -- be a bit behind if some other client submitted patches in parallel.

    , pobOperations :: ![Operation]
      -- ^ The operations which the client wants to store in the database.
    } deriving (Generic)

instance FromJSON PatchObjectBody where
    parseJSON (A.Object o) = PatchObjectBody <$> o .: "revisionId" <*> o .: "operations"
    parseJSON _            = fail "PatchObjectBody"


data PatchObjectResponse = PatchObjectResponse
    { porPreviousPatches :: ![Patch]
      -- ^ Patches which were already in the database. The submitted ops were
      -- rebased on top of these.

    , porNumProcessedOperations :: !Int
      -- ^ The number of operations which were processed. This may be smaller
      -- than the number of submitted ops if the processing failed somewhere
      -- in the middle. The client can then decide what to do with those which
      -- were not accepted.

    , porResultingPatches :: ![Patch]
      -- ^ Out of the submitted operations, these are the patches which were
      -- actually applied and stored in the database. This list may be shorter
      -- if some operations were dropped (because redundant or conflicting).
    } deriving (Generic)

instance ToJSON PatchObjectResponse where
    toJSON x = object
        [ "previousPatches" .= porPreviousPatches x
        , "numProcessedOperations" .= porNumProcessedOperations x
        , "resultingPatches" .= porResultingPatches x
        ]



--------------------------------------------------------------------------------
-- ObjectChanges

data ObjectChangeNotification
    = PatchNotification !Patch
      -- ^ A new patch was created.
    deriving (Generic)

    -- LatestReleaseNotification !Release
    -- A new release was created.

instance ToJSON ObjectChangeNotification where
    toJSON (PatchNotification p) = object [ "type" .= ("patch" :: Text), "content" .= p ]
    -- toJSON (ReleaseNotification r) = object [ "type" .= ("release" :: Text), "content" .= r ]



--------------------------------------------------------------------------------
-- LookupPatch

type LookupPatchResponse = Patch



--------------------------------------------------------------------------------
-- CreateRelease

data CreateReleaseBody = CreateReleaseBody
    {
    } deriving (Generic)

instance FromJSON CreateReleaseBody where
    parseJSON = undefined


data CreateReleaseResponse = CreateReleaseResponse
    {
    } deriving (Generic)

instance ToJSON CreateReleaseResponse where
    toJSON = undefined



--------------------------------------------------------------------------------
-- LookupRelease

data LookupReleaseResponse = LookupReleaseResponse
    {
    } deriving (Generic)

instance ToJSON LookupReleaseResponse where
    toJSON = undefined



--------------------------------------------------------------------------------
-- LookupLatestRelease

data LookupLatestReleaseResponse = LookupLatestReleaseResponse
    {
    } deriving (Generic)

instance ToJSON LookupLatestReleaseResponse where
    toJSON = undefined



--------------------------------------------------------------------------------
-- CreateSession

data CreateSessionBody = CreateSessionBody
    { csbLogin :: !SecretId
    , csbSecret :: !Text
    } deriving (Generic)

instance FromJSON CreateSessionBody where
    parseJSON (A.Object o) = CreateSessionBody <$> o .: "login" <*> o .: "secret"
    parseJSON _            = fail "CreateSessionBody"


data CreateSessionResponse = CreateSessionResponse
    { csrSessionId :: !SessionId
    , csrSessionObjId :: !ObjId
    } deriving (Generic)

instance ToJSON CreateSessionResponse where
    toJSON x = object
        [ "id" .= csrSessionId x
        , "objId" .= csrSessionObjId x
        ]



--------------------------------------------------------------------------------
-- LookupSession

data LookupSessionResponse = LookupSessionResponse
    { lsrSessionId :: !SessionId
    , lsrSessionObjId :: !ObjId
    } deriving (Generic)

instance ToJSON LookupSessionResponse where
    toJSON x = object
        [ "id" .= lsrSessionId x
        , "objId" .= lsrSessionObjId x
        ]



--------------------------------------------------------------------------------
-- Feed

data ChangeFeedSubscription
    = IncludeObjectChanges ObjId

instance FromJSON ChangeFeedSubscription where
    parseJSON (Array a) = case V.toList a of
        ["+", objId] -> IncludeObjectChanges <$> parseJSON objId
        _            -> fail "ChangeFeedSubscription"

    parseJSON _ = fail "ChangeFeedSubscription"



--------------------------------------------------------------------------------
-- ChangeSecret

data ChangeSecretBody = ChangeSecretBody
    { csbNewSecret :: !Text
    } deriving (Generic)

instance FromJSON ChangeSecretBody where
    parseJSON (A.Object o) = ChangeSecretBody <$> o .: "secret"
    parseJSON _            = fail "ChangeSecretBody"



--------------------------------------------------------------------------------
-- UploadBlob

newtype BlobContent = BlobContent ByteString

instance MimeUnrender OctetStream BlobContent where
    mimeUnrender _ bs = Right (BlobContent $ toStrict bs)

instance MimeRender OctetStream BlobContent where
    mimeRender _ (BlobContent bs) = fromStrict bs

data UploadBlobResponse = UploadBlobResponse
    { ubrId :: !BlobId
    , ubrSize :: !Int
    , ubrContentType :: !Text
    } deriving (Generic)

instance ToJSON UploadBlobResponse where
    toJSON x = object
        [ "id" .= ubrId x
        , "size" .= ubrSize x
        , "contentType" .= ubrContentType x
        ]



--------------------------------------------------------------------------------
-- LookupBlob

data LookupBlobResponse = LookupBlobResponse
    { lbrId :: !BlobId
    , lbrSize :: !Int
    , lbrContentType :: !Text
    } deriving (Generic)

instance ToJSON LookupBlobResponse where
    toJSON x = object
        [ "id" .= lbrId x
        , "size" .= lbrSize x
        , "contentType" .= lbrContentType x
        ]



--------------------------------------------------------------------------------
-- Signup

data SignupBody = SignupBody
    {
    } deriving (Generic)

instance FromJSON SignupBody where
    parseJSON = undefined

data SignupResponse = SignupResponse
    {
    } deriving (Generic)

instance ToJSON SignupResponse where
    toJSON = undefined
