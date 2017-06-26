{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE RecordWildCards     #-}

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE FlexibleContexts    #-}

module Avers.Server
    ( serveAversAPI

    , credentialsObjId

    , module Avers.Server.Authorization
    ) where


import           Control.Monad
import           Control.Monad.Except

import           Control.Concurrent
import           Control.Concurrent.STM

import           Data.Text              (Text)
import qualified Data.Text.Encoding     as T

import qualified Data.ByteString.Lazy   as LBS

import qualified Data.Set               as S


import           Data.Monoid
import           Data.Time
import           Data.Aeson (ToJSON, encode, decode)

import           Data.ByteArray.Encoding (Base(Base64), convertToBase)
import           Crypto.Hash (Digest, SHA3_256, hashlazy)

import           Servant.API hiding (Patch)
import           Servant.Server

import           Avers
import           Avers.API

import           Avers.Server.Authorization
import           Avers.Server.Instances ()

import           Network.HTTP.Types.Status

import           Network.Wai
import           Network.Wai.Handler.WebSockets (websocketsApp)
import qualified Network.WebSockets as WS

import           Web.Cookie

import           Prelude



etagVersion :: Text
etagVersion = "v0"


-- | Convert the 'Credentials' into an 'ObjId' to which the ceredentials refer.
-- That's the object the client is authenticated as.
credentialsObjId :: Handle -> Credentials -> Handler ObjId
credentialsObjId aversH cred = do
    errOrObjId <- case cred of
        SessionIdCredential sId -> liftIO $ evalAvers aversH $
            sessionObjId <$> lookupSession sId

    case errOrObjId of
        Left _  -> throwError err401
        Right s -> pure s


failWith :: Text -> Handler b
failWith e = throwError $ err500
    { errBody = LBS.fromChunks [T.encodeUtf8 e] }


aversResult :: Either AversError a -> Handler a
aversResult res = case res of
    Left e -> case e of
        DatabaseError detail                  -> failWith $ "database " <> detail
        NotAuthorized                         -> failWith $ "Unauthorized"
        DocumentNotFound _                    -> failWith $ "NotFound"
        UnknownObjectType detail              -> failWith $ "unknown object " <> detail
        ObjectNotFound _                      -> failWith $ "NotFound"
        ParseError _ detail                   -> failWith $ "parse " <> detail
        PatchError (UnknownPatchError detail) -> failWith $ "patch " <> detail
        AversError detail                     -> failWith $ "avers " <> detail
        InternalError ie                      -> aversResult (Left ie)
    Right r -> pure r


reqAvers :: Handle -> Avers a -> Handler a
reqAvers aversH m = liftIO (evalAvers aversH m) >>= aversResult


cacheableResponse :: (ToJSON a) => Maybe Text -> a -> Handler (Cacheable a)
cacheableResponse mbValidationToken a = do
    let etag = T.decodeUtf8 $ convertToBase Base64 $ (hashlazy (encode a) :: Digest SHA3_256)
    if mbValidationToken == Just (etagVersion <> ":" <> etag)
        then throwError err304
        else pure $ addHeader "no-cache, public, max-age=63072000"
                  $ addHeader (etagVersion <> ":" <> etag) a


serveAversAPI :: Handle -> Authorizations -> Server AversAPI
serveAversAPI aversH auth =
         serveCreateObject
    :<|> serveLookupObject
    :<|> servePatchObject
    :<|> serveDeleteObject
    :<|> serveLookupPatch
    :<|> serveObjectChanges
    :<|> serveCreateRelease
    :<|> serveLookupRelease
    :<|> serveLookupLatestRelease
    :<|> serveFeed
    :<|> serveChangeSecret
    :<|> serveCreateSession
    :<|> serveLookupSession
    :<|> serveDeleteSession
    :<|> serveUploadBlob
    :<|> serveLookupBlob
    :<|> serveLookupBlobContent

  where

    ----------------------------------------------------------------------------
    serveCreateObject :: Server CreateObject
    serveCreateObject cred body = do
        let objType = cobType body

        runAuthorization aversH $
            createObjectAuthz auth cred objType


        createdBy <- credentialsObjId aversH cred
        objId <- reqAvers aversH $ do
            SomeObjectType ot <- lookupObjectType objType
            content <- case parseValueAs ot (cobContent body) of
                Left e  -> throwError e
                Right x -> pure x

            createObject ot createdBy content

        pure $ CreateObjectResponse objId (cobType body) (cobContent body)


    ----------------------------------------------------------------------------
    serveLookupObject :: Server LookupObject
    serveLookupObject objId cred validationToken = do
        runAuthorization aversH $
            lookupObjectAuthz auth cred objId


        (Object{..}, Snapshot{..}) <- reqAvers aversH $ do
            object <- lookupObject objId
            snapshot <- lookupLatestSnapshot (BaseObjectId objId)

            pure (object, snapshot)

        cacheableResponse validationToken $ LookupObjectResponse
            { lorId = objId
            , lorType = objectType
            , lorCreatedAt = objectCreatedAt
            , lorCreatedBy = objectCreatedBy
            , lorRevisionId = snapshotRevisionId
            , lorContent = snapshotContent
            }


    ----------------------------------------------------------------------------
    servePatchObject :: Server PatchObject
    servePatchObject objId cred body = do
        runAuthorization aversH $
            patchObjectAuthz auth cred objId (pobOperations body)


        authorObjId <- credentialsObjId aversH cred
        (previousPatches, numProcessedOperations, resultingPatches) <- reqAvers aversH $ do
            applyObjectUpdates
                (BaseObjectId objId)
                (pobRevisionId body)
                authorObjId
                (pobOperations body)
                False

        pure $ PatchObjectResponse
            { porPreviousPatches = previousPatches
            , porNumProcessedOperations = numProcessedOperations
            , porResultingPatches = resultingPatches
            }


    ----------------------------------------------------------------------------
    serveDeleteObject :: Server DeleteObject
    serveDeleteObject objId cred = do
        runAuthorization aversH $
            deleteObjectAuthz auth cred objId

        throwError err501


    ----------------------------------------------------------------------------
    serveLookupPatch :: Server LookupPatch
    serveLookupPatch objId revId _cred validationToken = do
        -- TODO: authorization

        patch <- reqAvers aversH $ lookupPatch (BaseObjectId objId) revId
        cacheableResponse validationToken patch


    ----------------------------------------------------------------------------
    serveObjectChanges :: Server ObjectChanges
    serveObjectChanges objId _cred = Tagged $ \req respond -> respond $
        case websocketsApp WS.defaultConnectionOptions wsApp req of
            Nothing  -> responseLBS status500 [] "Failed"
            Just res -> res
      where
        wsApp pendingConnection = do
            connection <- WS.acceptRequest pendingConnection
            WS.forkPingThread connection 10

            chan <- changeChannel aversH
            loop connection chan

        loop :: WS.Connection -> TChan Change -> IO ()
        loop connection chan = do
            change <- atomically $ readTChan chan
            case change of
                (CPatch patch) -> when (patchObjectId patch == BaseObjectId objId) $
                    WS.sendTextData connection (encode patch)

            loop connection chan


    ----------------------------------------------------------------------------
    serveCreateRelease :: Server CreateRelease
    serveCreateRelease _ _ _ = do
        throwError err501


    ----------------------------------------------------------------------------
    serveLookupRelease :: Server LookupRelease
    serveLookupRelease _ _ _ _ = do
        throwError err501


    ----------------------------------------------------------------------------
    serveLookupLatestRelease :: Server LookupLatestRelease
    serveLookupLatestRelease _ _ _ = do
        throwError err501


    ----------------------------------------------------------------------------
    serveFeed :: Server Feed
    serveFeed _cred = Tagged $ \req respond -> respond $
        case websocketsApp WS.defaultConnectionOptions wsApp req of
            Nothing  -> responseLBS status500 [] "This is a WebSocket endpoint"
            Just res -> res
      where
        wsApp pendingConnection = do
            subscriptions <- newTVarIO S.empty
            connection <- WS.acceptRequest pendingConnection
            WS.forkPingThread connection 10

            void $ forkIO $ forever $ do
                msg <- WS.receiveData connection
                case decode msg of
                    Nothing -> pure ()
                    Just (IncludeObjectChanges objId) ->
                        atomically $ modifyTVar' subscriptions $ S.insert $ BaseObjectId objId

            chan <- changeChannel aversH
            loop connection subscriptions chan

        loop :: WS.Connection -> TVar (S.Set ObjectId) -> TChan Change -> IO ()
        loop connection subscriptions chan = do
            change <- atomically $ readTChan chan
            subs <- atomically $ readTVar subscriptions
            case change of
                (CPatch p) -> when (S.member (patchObjectId p) subs) $
                    WS.sendTextData connection (encode change)

            loop connection subscriptions chan


    ----------------------------------------------------------------------------
    serveChangeSecret :: Server ChangeSecret
    serveChangeSecret cred ChangeSecretBody{..} = do
        reqAvers aversH $ do
            Session{..} <- case cred of
                SessionIdCredential sId -> lookupSession sId

            updateSecret (SecretId $ unObjId sessionObjId) csbNewSecret


    ----------------------------------------------------------------------------
    sessionCookieName     = "session"
    sessionExpirationTime = 2 * 365 * 24 * 60 * 60

    mkSetCookie :: SessionId -> Handler SetCookie
    mkSetCookie sId = do
        now <- liftIO $ getCurrentTime
        pure $ def
            { setCookieName = sessionCookieName
            , setCookieValue = T.encodeUtf8 (unSessionId sId)
            , setCookiePath = Just "/"
            , setCookieExpires = Just $ addUTCTime sessionExpirationTime now
            , setCookieHttpOnly = True
            }

    serveCreateSession :: Server CreateSession
    serveCreateSession body = do
        -- Verify the secret, fail if it is invalid.
        reqAvers aversH $ verifySecret (csbLogin body) (csbSecret body)

        -- Create a new Session and save it in the database.
        now <- liftIO $ getCurrentTime
        sessId <- SessionId <$> liftIO (newId 80)
        -- isSecure <- rqIsSecure <$> getRequest

        let session = Session sessId (ObjId $ unSecretId $ csbLogin body) now now
        reqAvers aversH $ saveSession session

        setCookie <- mkSetCookie sessId
        pure $ addHeader setCookie $ CreateSessionResponse
            { csrSessionId = sessId
            , csrSessionObjId = ObjId $ unSecretId $ csbLogin body
            }


    ----------------------------------------------------------------------------
    serveLookupSession :: Server LookupSession
    serveLookupSession sId = do
        session <- reqAvers aversH $ lookupSession sId

        setCookie <- mkSetCookie sId
        pure $ addHeader setCookie $ LookupSessionResponse
            { lsrSessionId = sessionId session
            , lsrSessionObjId = sessionObjId session
            }

    ----------------------------------------------------------------------------
    serveDeleteSession :: Server DeleteSession
    serveDeleteSession sId = do
        reqAvers aversH $ dropSession sId

        pure $ addHeader (def
            { setCookieName = sessionCookieName
            , setCookieExpires = Just $ UTCTime (ModifiedJulianDay 0) 0
            }) ()


    ----------------------------------------------------------------------------
    serveUploadBlob :: Server UploadBlob
    serveUploadBlob cred mbContentType (BlobContent body) = do
        cType <- case mbContentType of
            Nothing -> throwError err400
            Just x -> pure x

        runAuthorization aversH $
            uploadBlobAuthz auth cred cType

        (Blob bId size _) <- reqAvers aversH $ do
            Avers.createBlob (LBS.fromStrict body) cType

        pure $ UploadBlobResponse bId size cType


    ----------------------------------------------------------------------------
    serveLookupBlob :: Server LookupBlob
    serveLookupBlob blobId cred = do
        runAuthorization aversH $
            lookupBlobAuthz auth cred blobId

        (Blob _ size cType) <- reqAvers aversH $ do
            Avers.lookupBlob blobId

        pure $ LookupBlobResponse blobId size cType


    ----------------------------------------------------------------------------
    serveLookupBlobContent :: Server LookupBlobContent
    serveLookupBlobContent blobId cred = do
        runAuthorization aversH $
            lookupBlobContentAuthz auth cred blobId

        throwError err501
