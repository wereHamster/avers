{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

{-|

This module abstracts the storage engine and provides functions to
manipulate objects stored in it.

-}

module Avers.Storage where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM

import           Control.Monad.Random (getRandomR, evalRandIO)
import           Control.Monad.State
import           Control.Monad.Except

import           Data.Pool
import           Data.Char
import           Data.Monoid
import           Data.Maybe
import qualified Data.ByteString.Lazy   as BL

import           Data.Vector        (Vector)
import qualified Data.Vector        as V

import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

import qualified Data.Map           as M

import           Data.Time
import           Data.List (find)
import           Data.Aeson (Value)
import           Data.Aeson.Types (emptyObject)

import           Data.ByteArray.Encoding (Base(Base16), convertToBase)
import           Crypto.Hash (Digest, SHA3_256, hashlazy)

import qualified Database.RethinkDB as R

import           Crypto.Scrypt

import           Avers.Metrics
import           Avers.Metrics.Measurements
import           Avers.Types
import           Avers.Patching
import           Avers.TH
import           Avers.Views
import           Avers.Index

import           Avers.Storage.Backend
import           Avers.Storage.Expressions

import           Prelude


requireResult :: AversError -> Maybe a -> Avers a
requireResult err Nothing  = throwError err
requireResult _   (Just v) = return v



-- | True if the object exists.
exists :: ObjId -> Avers Bool
exists objId = measureDuration M_avers_storage_exists_duration $
    existsDocument objectsTable (BaseObjectId objId)


-- | Lookup an 'Object' by its 'ObjId'. Throws 'ObjectNotFound' if the object
-- doesn't exist.
lookupObject :: ObjId -> Avers Object
lookupObject objId = measureDuration M_avers_storage_lookupObject_duration $ do
    mbObject <- lookupDocument objectsTable (BaseObjectId objId)
    requireResult (ObjectNotFound objId) mbObject


-- | Create a new object of the given type. An initial snapshot ('RevId' 0)
-- is created from the supplied content.
createObject :: (ToJSON a) => ObjectType a -> ObjId -> a -> Avers ObjId
createObject ot@ObjectType{..} createdBy content = do
    objId <- otId
    now   <- liftIO $ getCurrentTime

    createObject' objId now ot createdBy content

    return objId


-- | A more low-level version of 'createObject', for use when you want to
-- generate your own ObjId or create objects at a specific time.
createObject' :: (ToJSON a) => ObjId -> UTCTime -> ObjectType a -> ObjId -> a -> Avers ()
createObject' objId now ot@ObjectType{..} createdBy content = do
    insertDocument objectsTable object
    insertDocument patchesTable patch

    updateObjectViews ot objId (Just content)

  where
    object = Object objId otType now createdBy Nothing
    boId   = BaseObjectId objId
    op     = Set rootPath (Just $ toJSON content)
    patch  = Patch boId zeroRevId objId now op



-- | Mark the object as deleted.
deleteObject :: ObjId -> Avers ()
deleteObject objId = do
    obj <- lookupObject objId
    upsertDocument objectsTable (obj { objectDeleted = Just True })

    SomeObjectType objType <- lookupObjectType (objectType obj)
    updateObjectViews objType objId Nothing



-- | Prune the object from the database. This is only allowed if the object is
-- marked as deleted. Note that this is a very dangerous operation, it can not
-- be undone.
--
-- TODO: Prune related Release and Authoriation objects.
pruneObject :: ObjId -> Avers ()
pruneObject objId = do
    obj <- lookupObject objId
    SomeObjectType objType <- lookupObjectType (objectType obj)

    unless (objectDeleted obj == Just True) $
        strErr $ "pruneObject: object " ++ show objId ++ " is not deleted"

    -- Drop all base object patches and snapshots.
    void $ runQuery $ R.Delete $
        objectPatchSequenceE (BaseObjectId objId) 0 maxBound
    void $ runQuery $ R.Delete $
        objectSnapshotSequenceE (BaseObjectId objId) 0 maxBound

    -- Remove related entries from all views.
    forM_ (otViews objType) $ \(SomeView view) -> do
        deleteDocument (viewTable view) objId

    -- Drop the object itself.
    void $ deleteDocument objectsTable objId



-- | Create a checkpoint for for the given object. All patches (and of course
-- snapshots) before the checkpoint can be dropped. Use 'vacuumObject' to do
-- that.
createCheckpoint :: ObjectId -> ObjId -> Avers RevId
createCheckpoint objId createdBy = do
    now <- liftIO $ getCurrentTime
    Snapshot{..} <- lookupLatestSnapshot objId

    let op    = Set rootPath (Just $ toJSON snapshotContent)
        revId = succ snapshotRevisionId

    savePatch $ Patch objId revId createdBy now op
    saveSnapshot $ Snapshot snapshotObjectId revId snapshotContent

    return revId



isCheckpointPatch :: Patch -> Bool
isCheckpointPatch Patch{..} = case patchOperation of
    Set{..} -> opPath == rootPath
    _       -> False


-- This function is rather inefficient when the object has many patches,
-- because it fetches *all* patches to the client and does the search in
-- Haskell. A more efficient version would filter the patches on the server.
latestCheckpointPatch :: ObjectId -> Avers (Maybe Patch)
latestCheckpointPatch objId = do
    patches <- objectPatches objId
    return $ find isCheckpointPatch (reverse patches)



-- | Drop all patches and snapshots before the most recent checkpoint. This
-- effectively drops the object's history, and frees space in the database.
vacuumObject :: ObjectId -> Avers ()
vacuumObject objId = do
    mbPatch <- latestCheckpointPatch objId
    case mbPatch of
        Nothing -> return ()
        Just Patch{..} -> do
            -- This is the revision up to which we can drop snapshots and
            -- patches.
            let revId = unRevId patchRevisionId - 1

            -- Delete all snapshots and patches before the checkpoint.
            void $ runQuery $ R.Delete $
                objectSnapshotSequenceE objId 0 revId
            void $ runQuery $ R.Delete $
                objectPatchSequenceE objId 0 revId



-----------------------------------------------------------------------------
-- | Fetch the content of the object and try to parse it.
--
-- This function will fail with a 'ParseError' if the content can not be
-- decoded into the desired type.

objectContent :: (FromJSON a) => ObjectId -> Avers a
objectContent objId = do
    Snapshot{..} <- lookupLatestSnapshot objId
    parseValue snapshotContent



-----------------------------------------------------------------------------
-- | Get the snapshot of the newest revision of the given object.

lookupLatestSnapshot :: ObjectId -> Avers Snapshot
lookupLatestSnapshot objId = measureDuration M_avers_storage_lookupLatestSnapshot_duration $ do
    snapshot <- newestSnapshot objId
    patches  <- patchesAfterRevision objId (snapshotRevisionId snapshot)
    applyPatches snapshot patches


-- TODO: Verify that the patch is applicable to the snapshot, ie.
--   snapshot revId == patch revId + 1
applyPatchToSnapshot :: Snapshot -> Patch -> Avers Snapshot
applyPatchToSnapshot snapshot@Snapshot{..} Patch{..} =
    case applyOperation snapshotContent patchOperation of
        Left  e -> patchError e
        Right v -> return $ snapshot { snapshotContent    = v
                                     , snapshotRevisionId = patchRevisionId
                                     }


applyPatches :: Snapshot -> [Patch] -> Avers Snapshot
applyPatches snapshot patches =
    foldM applyPatchToSnapshot snapshot patches


lookupRecentRevision :: ObjectId -> Avers (Maybe RevId)
lookupRecentRevision objId = do
    m <- liftIO . atomically . readTVar =<< gets hRecentRevisionCache
    return $ M.lookup objId m

updateRecentRevision :: ObjectId -> RevId -> Avers ()
updateRecentRevision objId revId = do
    cache <- gets hRecentRevisionCache
    liftIO $ atomically $ modifyTVar' cache $
        M.insertWith max objId revId


-- | Lookup the latest snapshot within the given range. The bounds are
-- inclusive.
latestSnapshotBetween :: ObjectId -> Int -> Int -> Avers Snapshot
latestSnapshotBetween objId lo hi = do
    snapshots <- runQueryCollect $ limitE 1 $
        R.OrderByIndexed (R.Descending "objectSnapshotSequence") $
        objectSnapshotSequenceE objId lo hi

    snapshot <- case snapshots V.!? 0 of
        Just x  -> parseDatum x
        Nothing -> return $ initialSnapshot objId

    updateRecentRevision objId (snapshotRevisionId snapshot)
    return snapshot


-----------------------------------------------------------------------------
-- | Get the newest snapshot which is stored in the database. The object may
-- be at a higher revision if the later snapshots are missing from the
-- database.
--
-- This is an internal function. If you want the latest snapshot, you should
-- use 'lookupLatestSnapshot'.

newestSnapshot :: ObjectId -> Avers Snapshot
newestSnapshot objId = measureDuration M_avers_storage_newestSnapshot_duration $ do
    (RevId revId) <- fromMaybe zeroRevId <$> lookupRecentRevision objId
    latestSnapshotBetween objId revId maxBound


-- | Lookup the snapshot at a particular revision.
lookupSnapshot :: ObjectId -> RevId -> Avers Snapshot
lookupSnapshot objId (RevId revId) = measureDuration M_avers_storage_lookupSnapshot_duration $ do
    snapshot <- latestSnapshotBetween objId 0 revId

    -- Get all patches which we need to apply on top of the snapshot to
    -- arrive at the desired revision.
    patches <- patchesAfterRevision objId (snapshotRevisionId snapshot)

    -- Apply those patches to the snapshot.
    foldM applyPatchToSnapshot snapshot $
        filter (\Patch{..} -> unRevId patchRevisionId <= revId) patches



savePatch :: Patch -> Avers ()
savePatch = insertDocument patchesTable

saveSnapshot :: Snapshot -> Avers ()
saveSnapshot = insertDocument snapshotsTable



-----------------------------------------------------------------------------
-- Update the secret value. If a secret with the given 'SecretId' does not
-- exist yet, one will be created (IOW this function has upsert semantics).

updateSecret :: SecretId -> Text -> Avers ()
updateSecret secId secret = do
    ep <- liftIO $ encryptPassIO defaultParams (Pass $ T.encodeUtf8 secret)
    saveSecretValue secId ep



-----------------------------------------------------------------------------
-- | Verify the value against the secret. If that fails, then this function
-- throws an error.
--
-- This function automatically updates the secret in the database if the
-- scrypt params have changed.

verifySecret :: SecretId -> Text -> Avers ()
verifySecret secId secret = do
    Secret{..} <- requireResult (DocumentNotFound $ toPk secId) =<<
        lookupDocument secretsTable secId

    let ep        = EncryptedPass $ T.encodeUtf8 secretValue
    let (ok, new) = verifyPass defaultParams (Pass $ T.encodeUtf8 secret) ep

    -- If the secret doens't match, return NotFound
    when (not ok) $ strErr $ "Not Found"

    -- Update the database if we got a new value from the verify step. This
    -- is the case when the scrypt params have changed. See documentation of
    -- 'verifyPass'.
    maybe (return ()) (saveSecretValue secId) new



-----------------------------------------------------------------------------
-- | Internal function which actually saves a secret in the database.

saveSecretValue :: SecretId -> EncryptedPass -> Avers ()
saveSecretValue secId (EncryptedPass x) = do
   upsertDocument secretsTable $ Secret secId $ T.decodeUtf8 x


objectPatches :: ObjectId -> Avers [Patch]
objectPatches objId = patchesAfterRevision objId (RevId (-1))


patchesAfterRevision :: ObjectId -> RevId -> Avers [Patch]
patchesAfterRevision objId (RevId revId) = measureDuration M_avers_storage_patchesAfterRevision_duration $ do
    res <- runQueryCollect $
        R.OrderBy [R.Ascending "revisionId"] $
        objectPatchSequenceE objId (revId + 1) maxBound

    V.toList <$> V.mapM parseDatum res


lookupPatch :: ObjectId -> RevId -> Avers Patch
lookupPatch objId revId = measureDuration M_avers_storage_lookupPatch_duration $ do
    runQuerySingleSelection $ R.Get patchesTable $ R.lift $
        toPk objId <> "@" <> toPk revId



-- | Lookup an object type which is registered in the Avers monad.
lookupObjectType :: Text -> Avers SomeObjectType
lookupObjectType objType = do
    types <- objectTypes <$> gets hConfig
    case find (\(SomeObjectType ObjectType{..}) -> otType == objType) types of
        Nothing -> throwError $ UnknownObjectType objType
        Just x  -> return x



applyObjectUpdates
    :: ObjectId    -- ^ The object which you want to update
    -> RevId       -- ^ The 'RevId' against which the operations were created
    -> ObjId       -- ^ Committer
    -> [Operation] -- ^ The operations to apply
    -> Bool        -- ^ True if validation should be skipped
    -> Avers ([Patch], Int, [Patch])

applyObjectUpdates objId revId committerId ops novalidate = measureDuration M_avers_storage_applyObjectUpdates_duration $ do
    -- First check that the object exists. We'll need its metadata later.
    obj <- lookupObject baseObjId
    SomeObjectType ot <- lookupObjectType (objectType obj)

    -- The 'Snapshot' against which the submitted operations were created.
    baseSnapshot <- lookupSnapshot objId revId

    -- If there are any patches which the client doesn't know about we need
    -- to let her know.
    previousPatches <- patchesAfterRevision objId revId

    reportMeasurement M_avers_storage_applyObjectUpdates_numOperations
        (fromIntegral $ length ops)

    reportMeasurement M_avers_storage_applyObjectUpdates_numPreviousPatches
        (fromIntegral $ length previousPatches)

    latestSnapshot <- applyPatches baseSnapshot previousPatches

    -- Apply the operations and get the final snapshot.
    (Snapshot{..}, PatchState{..}) <- runStateT (patchHandler novalidate) $
        PatchState ot objId revId committerId ops 0
            baseSnapshot latestSnapshot previousPatches []

    -- Update object views.
    unless novalidate $ do
        content <- parseValue snapshotContent
        updateObjectViews ot baseObjId (Just content)

    return (previousPatches, psNumConsumedOperations, psPatches)

  where
    baseObjId = objectIdBase objId



data PatchState a = PatchState
  { psObjectType            :: ObjectType a
  , psObjectId              :: ObjectId
  , psRevisionId            :: RevId
  , psCommitterId           :: ObjId
  , psOperations            :: [ Operation ]
  , psNumConsumedOperations :: Int
  , psBaseSnapshot          :: Snapshot
  , psLatestSnapshot        :: Snapshot
  , psPreviousPatches       :: [ Patch ]
  , psPatches               :: [ Patch ]
  }


type AversPatch a b = StateT (PatchState a) Avers b


patchHandler :: (FromJSON a) => Bool -> AversPatch a Snapshot
patchHandler novalidate = do
    PatchState{..} <- get
    foldM (saveOperation $ snapshotContent psBaseSnapshot)
        psLatestSnapshot psOperations

  where

    saveOperation baseContent snapshot@Snapshot{..} op = do
        PatchState{..} <- get

        case rebaseOperation baseContent op psPreviousPatches of
            Nothing -> return snapshot
            Just op' -> do
                now <- liftIO $ getCurrentTime

                let revId = succ snapshotRevisionId
                    patch = Patch psObjectId revId psCommitterId now op'

                case applyOperation snapshotContent op' of
                    Left e -> error $ "Failure: " ++ (show e)
                    Right newContent
                        | newContent /= snapshotContent -> do
                            unless novalidate $ do
                                lift $ validateWithType psObjectType newContent

                            let newSnapshot = snapshot { snapshotContent    = newContent
                                                       , snapshotRevisionId = revId
                                                       }

                            -- Now we know that the patch can be applied cleanly, so
                            -- we can save it in the database.
                            lift $ savePatch patch

                            modify $ \s -> s
                                { psPatches = psPatches ++ [patch]
                                , psNumConsumedOperations = psNumConsumedOperations + 1
                                }

                            lift $ saveSnapshot newSnapshot
                            return newSnapshot
                        | otherwise -> return snapshot


existsBlob :: BlobId -> Avers Bool
existsBlob = existsDocument blobsTable

lookupBlob :: BlobId -> Avers Blob
lookupBlob bId = lookupDocument blobsTable bId >>=
    requireResult (DocumentNotFound $ toPk bId)

insertBlob :: Blob -> Avers ()
insertBlob = insertDocument blobsTable

saveBlobContent :: Blob -> BL.ByteString -> Avers ()
saveBlobContent Blob{..} content = do
    cfg <- gets hConfig
    res <- liftIO $ (putBlob cfg) blobId blobContentType content
    case res of
        Left e -> throwError e
        Right _ -> return ()


saveSession :: Session -> Avers ()
saveSession = insertDocument sessionsTable

lookupSession :: SessionId -> Avers Session
lookupSession sessId = lookupDocument sessionsTable sessId >>=
    requireResult (DocumentNotFound $ toPk sessId)

dropSession :: SessionId -> Avers ()
dropSession sessId = void $ deleteDocument sessionsTable sessId



newId :: Int -> IO Text
newId n = T.pack <$> take n <$> randomAlphanumericSequence
  where
    randomAlphanumericSequence :: IO String
    randomAlphanumericSequence = map alnum <$>
        evalRandIO (sequence $ repeat $ getRandomR (0, 61))

    alnum :: Int -> Char
    alnum x
        | x < 26 = chr ((x     ) + 65)
        | x < 52 = chr ((x - 26) + 97)
        | x < 62 = chr ((x - 52) + 48)
        | otherwise = error $ "Out of range: " ++ show x


validateObject :: Text -> Value -> Avers ()
validateObject objType value = do
    (SomeObjectType ot) <- lookupObjectType objType
    validateWithType ot value

validateWithType :: (FromJSON a) => ObjectType a -> Value -> Avers ()
validateWithType ot value = case parseValueAs ot value of
    Left e  -> throwError e
    Right _ -> return ()


lookupRelease :: ObjId -> RevId -> Avers Release
lookupRelease objId revId = objectContent (ReleaseObjectId objId revId)


-- | Create a new release of the given revision. If the object doesn't exist,
-- it will fail with 'ObjectNotFound'.
createRelease :: ObjId -> RevId -> Avers ()
createRelease objId revId = do
    -- The object must exist.
    objectExists <- exists objId
    unless objectExists $ throwError (ObjectNotFound objId)

    -- The release objects are not patchable at the moment. If one already
    -- exists, then there is nothing to do. Otherwise create the initial
    -- snapshot with empty content.
    exists' <- existsDocument snapshotsTable (toPk snapshot)
    unless exists' $ insertDocument snapshotsTable snapshot

  where
    objectId = ReleaseObjectId objId revId
    snapshot = Snapshot objectId zeroRevId emptyObject


lookupLatestRelease :: ObjId -> Avers (Maybe RevId)
lookupLatestRelease objId = do
    let match = R.lift $ "^" <> toPk objId <> "/release/"

        predicate :: R.Exp R.Object -> R.Exp Bool
        predicate x = R.Coerce
            (R.Match (objectFieldE "objectId" x) match)
            (R.lift ("bool"::Text))

    oId <- runQueryDatum $
        (objectFieldE "objectId" :: R.Exp R.Object -> R.Exp R.Datum) $
        headE $
        R.OrderBy [R.Descending "objectId"] $
        R.Filter predicate snapshotsTable

    case oId of
        ReleaseObjectId objId1 revId -> do
            when (objId /= objId1) $ databaseError "lookupLatestRelease: objId do not match"
            return $ Just revId

        _ -> return Nothing



-------------------------------------------------------------------------
-- Blobs
-------------------------------------------------------------------------

createBlob :: BL.ByteString -> Text -> Avers Blob
createBlob body contentType = do
    -- Only create the blob if it doesn't already exist. That way we can
    -- avoid having to upload the blob to the storage.
    ex <- existsBlob (blobId blob)
    unless ex $ do
        saveBlobContent blob body
        insertBlob blob

    return blob

  where
    size = fromIntegral $ BL.length body
    hash = convertToBase Base16 $ (hashlazy body :: Digest SHA3_256)
    blob = Blob (BlobId $ T.decodeUtf8 hash) size contentType


objectsOfType :: ObjectType a -> Avers (Vector ObjId)
objectsOfType objType = do
    let predicate :: R.Exp R.Object -> R.Exp Bool
        predicate = objectFieldEqE "type" (otType objType)

    res <- runQueryCollect $
        R.Map mapId $
        R.OrderBy [R.Descending "createdAt", R.Ascending "id"] $
        R.Filter isNotDeleted $
        R.Filter predicate objectsTable

    return $ V.map ObjId res


allObjectsOfType :: ObjectType a -> Avers (Vector ObjId)
allObjectsOfType objType = do
    let predicate :: R.Exp R.Object -> R.Exp Bool
        predicate = objectFieldEqE "type" (otType objType)

    res <- runQueryCollect $
        R.Map mapId $
        R.OrderBy [R.Descending "createdAt", R.Ascending "id"] $
        R.Filter predicate objectsTable

    return $ V.map ObjId res


isNotDeleted :: R.Exp R.Object -> R.Exp Bool
isNotDeleted x = R.Any
    [ R.Not $ R.HasFields ["deleted"] x
    , objectFieldEqE "deleted" False x
    ]

mapId:: R.Exp R.Object -> R.Exp Text
mapId = R.GetField "id"




--------------------------------------------------------------------------------
-- | Bootstrap the Avers handle: Create necessary tables, indexes, views etc.
-- This operation is idempotent.

indexF :: R.Exp R.Object -> R.Exp (R.Array R.Datum)
indexF obj = R.lift [ R.GetField "objectId" obj, R.GetField "revisionId" obj ]


bootstrap :: Avers ()
bootstrap = do
    createTable "objects" []
    createTable "sessions" []
    createTable "blobs" []
    createTable "secrets" []

    createTable "patches"
        [ SomeIndex $ Index "objectPatchSequence" indexF
        ]

    createTable "snapshots"
        [ SomeIndex $ Index "objectSnapshotSequence" indexF
        ]

    types <- objectTypes <$> gets hConfig
    forM_ types $ \(SomeObjectType ObjectType{..}) -> do
        forM_ otViews $ \(SomeView v@View{..}) -> do
            createTable (viewTableName v) viewIndices

    return ()


createTable :: Text -> [SomeIndex] -> Avers ()
createTable name indices = do
    let table = R.Table Nothing $ R.lift name
    pool <- gets hDatabaseHandlePool
    db <- liftIO $ withResource pool $ \handle -> pure $ R.handleDatabase handle

    tables <- runQuery $ R.ListTables db
    when (name `V.notElem` tables) $ do
        liftIO $ putStrLn $ "Creating table '" <> T.unpack name <> "'"
        void $ runQuery $ R.CreateTable db (R.lift name)
        void $ runQuery $ R.WaitTable table

    existingIndices <- runQuery $ R.ListIndices table
    forM_ indices $ \(SomeIndex Index{..}) -> do
        when (indexName `V.notElem` existingIndices) $ do
            liftIO $ putStrLn $ "Creating index '" <> T.unpack indexName <> "' on table '" <> T.unpack name <> "'"
            void $ runQuery $ R.CreateIndex table (R.lift indexName) indexExpression
            void $ runQuery $ R.WaitIndex table [R.lift indexName]


-- | Stream new patches from the database into the channel.
streamPatches :: Pool R.Handle -> TChan Change -> IO ()
streamPatches pool chan = forever $ do
    withResource pool $ \handle -> do
        putStrLn $ "streamPatches: start"
        token <- R.start handle $ R.SequenceChanges patchesTable
        loop handle token
        putStrLn $ "streamPatches: done"

    -- We shouldn't reach this place too often. If we get here it's usually because
    -- something unexpected has happened in the system. Wait a bit before retrying.
    threadDelay $ 5 * 1000 * 1000

  where
    writePatchNotifications :: Vector R.ChangeNotification -> IO ()
    writePatchNotifications v = forM_ v $ \cn -> case parseDatum (R.cnNewValue cn) of
        Left e  -> putStrLn $ "streamPatches: failed to parse value - " <> show e
        Right p -> atomically $ writeTChan chan $ CPatch p

    loop :: R.Handle -> R.Token -> IO ()
    loop handle token = do
        res <- R.nextResult handle token :: IO (Either R.Error (R.Sequence R.ChangeNotification))
        case res of
            Left e                -> putStrLn $ "streamPatches: nextResult error - " <> show e
            Right (R.Done r)      -> writePatchNotifications r
            Right (R.Partial _ r) -> do
                writePatchNotifications r
                R.continue handle token
                loop handle token


-- | Return a 'TChan' to which all changes in the system are streamed. Make
-- sure to continuously drain items from the 'TChan', otherwise they will
-- accumulate in memory and you will run OOM eventually.
--
-- Do not write into the channel!
changeChannel :: Handle -> IO (TChan Change)
changeChannel h = atomically $ dupTChan (hChanges h)
