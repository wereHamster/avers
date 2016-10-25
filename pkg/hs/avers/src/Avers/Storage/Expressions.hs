{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}


{-|

This module contains RethinkDB Expressions which are used to build queries.

-}

module Avers.Storage.Expressions where


import qualified Data.Vector        as V

import           Data.Text (Text)
import           Database.RethinkDB as R

import           Avers.Types hiding (Object)

import           Prelude hiding (lookup)



-- All the tables which this storage engine uses.
objectsTable, sessionsTable, snapshotsTable, patchesTable, secretsTable, blobsTable :: Exp Table
blobsTable     = Table Nothing $ lift ("blobs" :: Text)
objectsTable   = Table Nothing $ lift ("objects" :: Text)
patchesTable   = Table Nothing $ lift ("patches" :: Text)
secretsTable   = Table Nothing $ lift ("secrets" :: Text)
sessionsTable  = Table Nothing $ lift ("sessions" :: Text)
snapshotsTable = Table Nothing $ lift ("snapshots" :: Text)


-- | The primary key in all our documents is the default "id".
primaryKeyField :: Text
primaryKeyField = "id"


-- | Expression which represents the primary key field.
primaryKeyFieldE :: Exp Text
primaryKeyFieldE = lift primaryKeyField


-- | Expression which represents the value of a field inside of an Object.
objectFieldE :: (IsDatum a) => Text -> Exp Object -> Exp a
objectFieldE field obj = GetField (lift field) obj


-- | True if the object field matches the given value.
objectFieldEqE :: (ToDatum a) => Text -> a -> Exp Object -> Exp Bool
objectFieldEqE field value obj = Eq
    (objectFieldE field obj :: Exp Datum)
    (lift $ toDatum value)


-- | True if the object's primary key matches the given string.
primaryKeyEqE :: Text -> Exp Object -> Exp Bool
primaryKeyEqE = objectFieldEqE primaryKeyField


-- | Take the first item out of a sequence. Beware that this throws an error
-- when the sequence is empty.
headE :: (IsSequence a, IsDatum r) => Exp a -> Exp r
headE = Nth 0


-- | Limit a sequence to the first 'n' items.
limitE :: (IsSequence s) => Int -> Exp s -> Exp s
limitE n s = Limit (fromIntegral n) s


mkBounds :: ObjectId -> Int -> Int -> (Bound, Bound)
mkBounds objId lo hi = (mkBound objId lo, mkBound objId hi)

mkBound :: ObjectId -> Int -> Bound
mkBound objId revId = Closed $ Array $ V.fromList
    [String $ toPk objId, Number $ fromIntegral revId]


objectSnapshotSequenceE :: ObjectId -> Int -> Int -> Exp Table
objectSnapshotSequenceE objId lo hi =
    BetweenIndexed "objectSnapshotSequence" (mkBounds objId lo hi) $
    snapshotsTable


objectPatchSequenceE :: ObjectId -> Int -> Int -> Exp Table
objectPatchSequenceE objId lo hi =
    BetweenIndexed "objectPatchSequence" (mkBounds objId lo hi) $
    patchesTable
