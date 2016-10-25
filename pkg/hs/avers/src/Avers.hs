{-# LANGUAGE NoImplicitPrelude #-}

module Avers
  (
    -- * The Avers Monad
    Avers
  , evalAvers

    -- * Types
  , Path(..)
  , Pk(..)

  , ObjId(..)
  , rootObjId

  , RevId(..)
  , zeroRevId

  , ObjectId(..)
  , Operation(..)

    -- * Object
  , Object(..)
  , exists
  , createObject
  , createObject'
  , lookupObject
  , deleteObject
  , pruneObject
  , objectsOfType
  , allObjectsOfType

  , createCheckpoint
  , vacuumObject

    -- * Patch
  , Patch(..)
  , PatchError(..)
  , lookupPatch

    -- * Snapshot
  , Snapshot(..)
  , lookupLatestSnapshot
  , objectContent

    -- * Release
  , Release(..)
  , lookupRelease
  , createRelease
  , lookupLatestRelease

    -- * Patching
  , resolvePathIn

    -- * Session
  , SessionId(..)
  , Session(..)
  , saveSession
  , lookupSession
  , dropSession

  , ObjectType(..)
  , SomeObjectType(..)
  , lookupObjectType

  , AversError(..)
  , Config(..)
  , Handle
  , newHandle
  , newState
  , strErr
  , parseValueAs

  , bootstrap

    -- * Blob
  , BlobId(..)
  , Blob(..)
  , createBlob
  , lookupBlob

    -- * Secret
  , SecretId(..)
  , Secret(..)
  , updateSecret
  , verifySecret

  , applyObjectUpdates
  , runQuery
  , runQueryCollect
  , parseValue
  , parseDatum
  , newId

  , objectsTable
  , blobsTable

  , validateObject

    -- * Views
  , View(..)
  , SomeView(..)
  , viewTable
  , updateView

    -- * Index
  , Index(..)
  , SomeIndex(..)

    -- * Metrics
  , Measurement(..)
  , measurementLabels

    -- * Change
  , Change(..)
  , changeChannel
  ) where



import Avers.Handle
import Avers.Index
import Avers.Metrics.Measurements
import Avers.Patching
import Avers.Storage
import Avers.Storage.Backend
import Avers.Storage.Expressions
import Avers.Types
import Avers.Views
