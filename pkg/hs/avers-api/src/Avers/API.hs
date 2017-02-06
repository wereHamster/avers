{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Avers.API
    ( AversAPI

    , CreateObject
    , LookupObject
    , PatchObject
    , DeleteObject
    , LookupPatch
    , ObjectChanges
    , CreateRelease
    , LookupRelease
    , LookupLatestRelease
    , Feed
    , ChangeSecret
    , CreateSession
    , LookupSession
    , DeleteSession
    , UploadBlob
    , LookupBlob
    , LookupBlobContent

    , CacheValidationToken, Cacheable

    , module Avers.API.Types
    , module Avers.API.Credentials
    ) where


import Data.Text            (Text)

import Avers.Types          (ObjId, RevId, BlobId, SessionId)

import Avers.API.Types
import Avers.API.Instances  ()
import Avers.API.Credentials

import Servant.API

import Web.Cookie



--------------------------------------------------------------------------------
-- General structure of endpoint definitions
--
-- The definition of an endpoint would be too much to put on a single line,
-- so it is split into multiple lines according to a fixed schema. Each line
-- represents a particular aspect of the request/response. Lines can be omitted
-- if they don't apply to the endpoint.
--
--  <path> including any captured components
--  <credentials>
--  <headers>
--  <cache validation token>
--  <request body>
--  <method and response>



-- | The cache validator token when passed in the request. The server will
-- use it to determine if the cached response on the client can be reused
-- or not.
type CacheValidationToken = Header "If-None-Match" Text


-- | Includes @Cache-Control@ and @ETag@ headers in the response to mark
-- it as cacheable by the client.
type Cacheable a = Headers '[Header "Cache-Control" Text, Header "ETag" Text] a



--------------------------------------------------------------------------------
-- | The complete Avers API


type CreateObject
    = "objects"
    :> Credentials
    :> ReqBody '[JSON] CreateObjectBody
    :> Post '[JSON] CreateObjectResponse

type LookupObject
    = "objects" :> Capture "objId" ObjId
    :> Credentials
    :> CacheValidationToken
    :> Get '[JSON] (Cacheable LookupObjectResponse)

type PatchObject
    = "objects" :> Capture "objId" ObjId
    :> Credentials
    :> ReqBody '[JSON] PatchObjectBody
    :> Patch '[JSON] PatchObjectResponse

type DeleteObject
    = "objects" :> Capture "objId" ObjId
    :> Credentials
    :> Delete '[JSON] ()

type LookupPatch
    = "objects" :> Capture "objId" ObjId :> "patches" :> Capture "revId" RevId
    :> Credentials
    :> CacheValidationToken
    :> Get '[JSON] (Cacheable LookupPatchResponse)

type ObjectChanges
    = "objects" :> Capture "objId" ObjId :> "changes"
    :> Credentials
    :> Raw -- WebSocket, stream of ObjectChangeNotification

type CreateRelease
    = "objects" :> Capture "objId" ObjId :> "releases"
    :> Credentials
    :> ReqBody '[JSON] CreateReleaseBody
    :> Post '[JSON] CreateReleaseResponse

type LookupRelease
    = "objects" :> Capture "objId" ObjId :> "releases" :> Capture "revId" RevId
    :> Credentials
    :> CacheValidationToken
    :> Get '[JSON] (Cacheable LookupReleaseResponse)

type LookupLatestRelease
    = "objects" :> Capture "objId" ObjId :> "releases" :> "_latest"
    :> Credentials
    :> CacheValidationToken
    :> Get '[JSON] (Cacheable LookupLatestReleaseResponse)

type Feed
    = "feed"
    :> Credentials
    :> Raw -- WebSocket, stream of 'Change' objects

type ChangeSecret
    = "secret"
    :> Credentials
    :> ReqBody '[JSON] ChangeSecretBody
    :> Post '[JSON] ()

type CreateSession
    = "session"
    :> ReqBody '[JSON] CreateSessionBody
    :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie] CreateSessionResponse)

type LookupSession
    = "session"
    :> SessionId
    :> Get '[JSON] (Headers '[Header "Set-Cookie" SetCookie] LookupSessionResponse)

type DeleteSession
    = "session"
    :> SessionId
    :> Delete '[JSON] (Headers '[Header "Set-Cookie" SetCookie] ())

type UploadBlob
    = "blobs"
    :> Credentials
    :> Header "Content-Type" Text
    :> ReqBody '[OctetStream] BlobContent
    :> Post '[JSON] UploadBlobResponse

type LookupBlob
    = "blobs" :> Capture "blobId" BlobId
    :> Credentials
    :> Get '[JSON] LookupBlobResponse

type LookupBlobContent
    = "blobs" :> Capture "blobId" BlobId :> "content"
    :> Credentials
    :> Get '[OctetStream] (Headers '[Header "Content-Type" Text] BlobContent)


--------------------------------------------------------------------------------
-- | The complete Avers API as a data type.

type AversAPI
    =    CreateObject
    :<|> LookupObject
    :<|> PatchObject
    :<|> DeleteObject
    :<|> LookupPatch
    :<|> ObjectChanges
    :<|> CreateRelease
    :<|> LookupRelease
    :<|> LookupLatestRelease
    :<|> Feed
    :<|> ChangeSecret
    :<|> CreateSession
    :<|> LookupSession
    :<|> DeleteSession
    :<|> UploadBlob
    :<|> LookupBlob
    :<|> LookupBlobContent
