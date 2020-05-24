{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}

module Avers.Types where


import           GHC.Generics

import           Control.Applicative

import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.State

import           Control.Concurrent.STM

import           Data.Time.Clock
import           Data.String
import           Data.ByteString.Lazy (ByteString)

import           Data.Text (Text)
import qualified Data.Text as T

import           Data.Map  (Map)

import           Data.Char

import           Data.Attoparsec.Text

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import           Data.Aeson (Value(String))
import           Data.Aeson.Types (parseEither)

import           Network.URI

import qualified Database.RethinkDB       as R
import           Database.RethinkDB.TH

import           Data.Pool

import           Avers.TH
import           Avers.Index
import           Avers.Metrics.Measurements



-----------------------------------------------------------------------------
-- | Pk - Types which can be converted to a database primary key.

class Pk a where
    toPk :: a -> Text

instance Pk Text where
    toPk = id



-----------------------------------------------------------------------------
-- | Path

newtype Path = Path { unPath :: Text }
    deriving (Eq, Ord, Show, Generic)

instance IsString Path where
    fromString = Path . T.pack

instance ToJSON Path where
    toJSON = toJSON . unPath

instance FromJSON Path where
    parseJSON (String s) = return $ Path s
    parseJSON _          = fail "Path"

instance R.FromDatum Path where
    parseDatum (R.String s) = return $ Path s
    parseDatum _            = fail "Path"

instance R.ToDatum Path where
    toDatum = R.toDatum . unPath


-- | This path refers to the root of an object. It is only used in 'Set'
-- operations.
rootPath :: Path
rootPath = Path ""



-----------------------------------------------------------------------------
-- | ObjId

newtype ObjId = ObjId { unObjId :: Text }
    deriving (Eq, Ord, Show, Generic)

instance Pk ObjId where
    toPk = unObjId

instance ToJSON ObjId where
    toJSON = toJSON . unObjId

instance FromJSON ObjId where
    parseJSON x = ObjId <$> parseJSON x

instance R.FromDatum ObjId where
    parseDatum x = ObjId <$> R.parseDatum x

instance R.ToDatum ObjId where
    toDatum = R.toDatum . unObjId


-- | The root object id is used for object created internally or when there
-- is no applicable creator.
rootObjId :: ObjId
rootObjId = ObjId ""



-----------------------------------------------------------------------------
-- | RevId

newtype RevId = RevId { unRevId :: Int }
    deriving (Eq, Ord, Show, Generic)

instance Enum RevId where
    succ (RevId x) = RevId (succ x)
    pred (RevId x) = RevId (pred x)
    toEnum         = RevId
    fromEnum       = unRevId

instance Pk RevId where
    toPk = T.pack . show . unRevId

instance ToJSON RevId where
    toJSON = toJSON . unRevId

instance FromJSON RevId where
    parseJSON x = RevId <$> parseJSON x

instance R.FromDatum RevId where
    parseDatum x = RevId <$> R.parseDatum x

instance R.ToDatum RevId where
    toDatum = R.toDatum . unRevId


-- | The 'RevId' which is used for the initial snapshot.
zeroRevId :: RevId
zeroRevId = RevId 0



-----------------------------------------------------------------------------
-- | ObjectId

data ObjectId

    = BaseObjectId !ObjId
      -- ^ The base object whose snapshots contain the actual content.

    | ReleaseObjectId !ObjId !RevId
     -- ^ An object describing a particualar release of the base object.

    | AuthorizationObjectId !ObjId
      -- ^ Object which contains authorization rules.

    deriving (Eq, Ord, Show, Generic)


instance Pk ObjectId where
    toPk (BaseObjectId objId)          = toPk objId
    toPk (ReleaseObjectId objId revId) = toPk objId <> "/release/" <> toPk revId
    toPk (AuthorizationObjectId objId) = toPk objId <> "/authorization"

instance ToJSON ObjectId where
    toJSON = toJSON . toPk

instance FromJSON ObjectId where
    parseJSON (String x) = either fail return $ parseOnly objectIdParser x
    parseJSON _          = fail "ObjectId"

instance R.FromDatum ObjectId where
    parseDatum (R.String x) = either fail return $ parseOnly objectIdParser x
    parseDatum _            = fail "ObjectId"

instance R.ToDatum ObjectId where
    toDatum = R.toDatum . toPk


objectIdParser :: Parser ObjectId
objectIdParser = (releaseObjectId <|> authorizationObjectId <|> baseObjectId) <* endOfInput
  where
    objId = ObjId <$> takeWhile1 isAlphaNum
    revId = RevId <$> decimal

    baseObjectId = BaseObjectId
        <$> objId

    releaseObjectId = ReleaseObjectId
        <$> objId
        <*  string "/release/"
        <*> revId

    authorizationObjectId = AuthorizationObjectId
        <$> objId
        <*  string "/authorization"


parseObjectId :: Text -> Maybe ObjectId
parseObjectId text = case parseOnly objectIdParser text of
    Left _  -> Nothing
    Right v -> Just v


objectIdBase :: ObjectId -> ObjId
objectIdBase (BaseObjectId          objId  ) = objId
objectIdBase (ReleaseObjectId       objId _) = objId
objectIdBase (AuthorizationObjectId objId  ) = objId



-----------------------------------------------------------------------------
-- | The operations that can be applied to JSON values.
data Operation
  -- | Set is applied to 'Object's. It is used for adding, updating
  --   and deleting properties from the object.
  = Set
    { opPath  :: !Path
    , opValue :: !(Maybe Value)
    }

  -- | Splice is used to manipulate 'Array's. It can remove and insert
  --   multiple elements in a single operation.
  | Splice
    { opPath   :: !Path
    , opIndex  :: !Int
    , opRemove :: !Int
    , opInsert :: ![ Value ]
    }

    deriving (Eq, Show, Generic)

$(deriveEncoding (deriveJSONOptions "op"){
    omitNothingFields = True,
    sumEncoding       = TaggedObject "type" "content"
} ''Operation)



data PatchError
    = UnknownPatchError !Text
    deriving (Show, Generic)

type PatchM a = Either PatchError a


data Object = Object
  { objectId            :: !ObjId
  , objectType          :: !Text
  , objectCreatedAt     :: !UTCTime
  , objectCreatedBy     :: !ObjId
  , objectDeleted       :: !(Maybe Bool)
  } deriving (Show, Generic)

instance Pk Object where
    toPk = toPk . objectId

$(deriveEncoding (deriveJSONOptions "object") ''Object)



-----------------------------------------------------------------------------
-- | Patch

data Patch = Patch
  { patchObjectId       :: !ObjectId
  , patchRevisionId     :: !RevId
  , patchAuthorId       :: !ObjId
  , patchCreatedAt      :: !UTCTime
  , patchOperation      :: !Operation
  } deriving (Show, Generic)

instance Pk Patch where
    toPk Patch{..} = toPk patchObjectId <> "@" <> toPk patchRevisionId

$(deriveEncoding (deriveJSONOptions "patch") ''Patch)



-----------------------------------------------------------------------------
-- | Snapshot

data Snapshot = Snapshot
  { snapshotObjectId    :: !ObjectId
  , snapshotRevisionId  :: !RevId
  , snapshotContent     :: !Value
  } deriving (Show, Generic)

instance Pk Snapshot where
    toPk Snapshot{..} = toPk snapshotObjectId <> "@" <> toPk snapshotRevisionId

$(deriveEncoding (deriveJSONOptions "snapshot") ''Snapshot)


-- | The initial snapshot on top of which all future patches are applied.
initialSnapshot :: ObjectId -> Snapshot
initialSnapshot objId = Snapshot objId (RevId (-1)) Aeson.emptyObject



-----------------------------------------------------------------------------
-- | Release

data Release = Release

instance ToJSON Release where
    toJSON = const Aeson.emptyObject

instance FromJSON Release where
    parseJSON (Aeson.Object _) = return Release
    parseJSON _                = fail "Release"

-- $(deriveEncoding (deriveJSONOptions "release") ''Release)



-----------------------------------------------------------------------------
-- | SecretId

newtype SecretId = SecretId { unSecretId :: Text }
    deriving (Show, Generic)

instance Pk SecretId where
    toPk = unSecretId

instance ToJSON SecretId where
    toJSON = toJSON . unSecretId

instance FromJSON SecretId where
    parseJSON x = SecretId <$> parseJSON x

instance R.FromDatum SecretId where
    parseDatum x = SecretId <$> R.parseDatum x

instance R.ToDatum SecretId where
    toDatum = R.toDatum . unSecretId



-----------------------------------------------------------------------------
-- | Secret
--
-- A 'Secret' is a password (encrypted with scrypt) that is attached to
-- a 'SecretId' (for example the 'ObjId' of an account).
--
-- It is up to you to ensure that 'SecretId's are unique. If you use 'ObjId's
-- then they by definition are.

data Secret = Secret
  { secretId    :: !SecretId
  , secretValue :: !Text
  } deriving (Generic)

instance Pk Secret where
    toPk = toPk . secretId

$(deriveEncoding (deriveJSONOptions "secret") ''Secret)



-----------------------------------------------------------------------------
-- | BlobId

newtype BlobId = BlobId { unBlobId :: Text }
    deriving (Show, Generic)

instance Pk BlobId where
    toPk = unBlobId

instance ToJSON BlobId where
    toJSON = toJSON . unBlobId

instance FromJSON BlobId where
    parseJSON x = BlobId <$> parseJSON x

instance R.FromDatum BlobId where
    parseDatum x = BlobId <$> R.parseDatum x

instance R.ToDatum BlobId where
    toDatum = R.toDatum . unBlobId



-----------------------------------------------------------------------------
-- | Blob

data Blob = Blob
  { blobId          :: !BlobId
  , blobSize        :: !Int
  , blobContentType :: !Text
  } deriving (Show, Generic)

instance Pk Blob where
    toPk = toPk . blobId

$(deriveEncoding (deriveJSONOptions "blob") ''Blob)



-----------------------------------------------------------------------------
-- | SessionId

newtype SessionId = SessionId { unSessionId :: Text }
    deriving (Generic)

instance Pk SessionId where
    toPk = unSessionId

instance ToJSON SessionId where
    toJSON = toJSON . unSessionId

instance FromJSON SessionId where
    parseJSON x = SessionId <$> parseJSON x

instance R.FromDatum SessionId where
    parseDatum x = SessionId <$> R.parseDatum x

instance R.ToDatum SessionId where
    toDatum = R.toDatum . unSessionId



-----------------------------------------------------------------------------
-- | The session record that is stored in the database.
--
-- A session is a unique identifier attached to a particular object. It
-- contains the creation date and when it was last accessed. If you need to
-- store additional data for a session, we recommend to use cookies.

data Session = Session
  { sessionId             :: !SessionId
  , sessionObjId          :: !ObjId
  , sessionCreatedAt      :: !UTCTime
  , sessionLastAccessedAt :: !UTCTime
  } deriving (Generic)

instance Pk Session where
    toPk Session{..} = toPk sessionId

$(deriveEncoding (deriveJSONOptions "session") ''Session)



data AversError
    = InternalError !AversError
    | DatabaseError !Text
    | PatchError !PatchError
    | ParseError !Value !Text
    | UnknownObjectType !Text
    | ObjectNotFound !ObjId
    | DocumentNotFound !Text
    | AversError !Text
    | NotAuthorized
    deriving (Show, Generic)


internalError :: AversError -> Avers a
internalError = throwError . InternalError

internal :: Avers a -> Avers a
internal m = m `catchError` internalError

databaseError :: Text -> Avers a
databaseError = throwError . DatabaseError

patchError :: PatchError -> Avers a
patchError = throwError . PatchError

parseError :: (MonadError AversError m) => Value -> Text -> m a
parseError value text = throwError $ ParseError value text

documentNotFound :: Text -> Avers a
documentNotFound = throwError . DocumentNotFound

strErr :: String -> Avers a
strErr = throwError . AversError . T.pack


-- | An 'ObjectType' describes a particular type of object that is managed by
-- Avers.
data ObjectType a = ObjectType
  { otType   :: !Text
    -- ^ The value of the @type@ field of the 'Object'.

  , otId     :: Avers ObjId
    -- ^ Action which generates a new id. This is so that object types can
    -- have different strategies how to generate ids.

  , otViews :: [SomeView a]
  }

data SomeObjectType where
     SomeObjectType :: (R.ToDatum a, R.FromDatum a, FromJSON a, ToJSON a)
        => ObjectType a -> SomeObjectType

parseValueAs :: (FromJSON a) => ObjectType a -> Value -> Either AversError a
parseValueAs ObjectType{..} value = case parseEither parseJSON value of
    Left  e -> parseError value (T.pack $ show e)
    Right x -> return x



-- | Configuration of the 'Avers' monad.
data Config = Config

    { databaseURI :: !URI
      -- ^ 'URI' which describes the connection details to the RethinkDB
      -- database. The 'URI' *MUST* include at least the hostname ('uriRegName')
      -- and database name ('uriPath' without the leading slash). The port
      -- ('uriPort') and credentials ('uriUserInfo') *MAY* be left empty.
      -- in that case the default port will be used.

    , putBlob :: BlobId -> Text -> ByteString -> IO (Either AversError ())
      -- ^ Function which saves the given blob in the blob store. This can be
      -- the local filesystem or an external service such as Amazon S3.

    , objectTypes :: ![SomeObjectType]
      -- ^ All the object types which Avers knows about.

    , emitMeasurement :: Measurement -> Double -> IO ()
      -- ^ This is called when the internal instrumentation code creates
      -- a measurement.
    }



-- | A change in the system, for example a new object, patch, release, blob etc.
data Change
    = CPatch !Patch -- ^ A new patch was created.
    deriving (Show, Generic)

instance ToJSON Change where
    toJSON (CPatch p) = Aeson.object [ "type" Aeson..= ("patch" :: Text), "content" Aeson..= p ]



data Handle = Handle
    { hConfig :: !Config
      -- ^ A reference to the config, just in case we need it.

    , hDatabaseHandlePool :: !(Pool R.Handle)
      -- ^ A pool of handles which are used to access the database.

    , hRecentRevisionCache :: !(TVar (Map ObjectId RevId))
      -- ^ Map from 'ObjectId' to a recent 'RevId'. It may be the latest or
      -- a few revisions behind.

    , hChanges :: !(TChan Change)
      -- ^ Changes in the system (new patches, objects, releases etc), even
      -- those created through other handles, are streamed into this channel.
      -- If you want to be informed of those changes, duplicate the channel
      -- and read from the copy.
    }



newtype Avers a = Avers
    { runAvers :: StateT Handle (ExceptT AversError IO) a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadError AversError, MonadState Handle)

class (Monad m) => MonadAvers m where
    liftAvers :: Avers a -> m a

instance MonadAvers Avers where
    liftAvers = id

instance MonadAvers m => MonadAvers (StateT s m) where
    liftAvers = lift . liftAvers

evalAvers :: Handle -> Avers a -> IO (Either AversError a)
evalAvers h m = runExceptT $ evalStateT (runAvers m) h



------------------------------------------------------------------------------
-- View

data View obj a = View
  { viewName :: Text
    -- ^ The table name is derived from the view name. Therefore it should
    -- be unique amongst all views.

  , viewParser :: R.Datum -> Either AversError a
    -- ^ Function which parses objects stored in this view.

  , viewObjectTransformer :: obj -> Avers (Maybe a)
    -- ^ Function which transforms an Avers Object into a type stored
    -- in the view.

  , viewIndices :: [SomeIndex]
    -- ^ Secondary indices defined on the view.
  }


data SomeView obj where
     SomeView :: (R.ToDatum a, R.FromDatum a, FromJSON obj, ToJSON a)
        => View obj a -> SomeView obj
