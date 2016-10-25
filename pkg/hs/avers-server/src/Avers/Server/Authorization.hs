module Avers.Server.Authorization
    ( Authorizations(..), Authz, AuthzR(..)
    , defaultAuthorizations

    , runAuthorization

    , trace
    , sufficient
    , requisite

    , sessionCreatedObject
    , sessionIsObject
    ) where


import Control.Monad.IO.Class
import Control.Monad.Except

import Data.Text (Text)

import Avers
import Avers.API

import Servant.Server




--------------------------------------------------------------------------------
-- | Defines all the authorization points which are used in the server. For
-- each you can supply your own logic. The default is to allow everything.

data Authorizations = Authorizations
    { createObjectAuthz :: Credentials -> Text -> Authz
    , lookupObjectAuthz :: Credentials -> ObjId -> Authz
    , patchObjectAuthz :: Credentials -> ObjId -> [Operation] -> Authz
    , deleteObjectAuthz :: Credentials -> ObjId -> Authz
    , uploadBlobAuthz :: Credentials -> Text -> Authz
    , lookupBlobAuthz :: Credentials -> BlobId -> Authz
    , lookupBlobContentAuthz :: Credentials -> BlobId -> Authz
    }


defaultAuthorizations :: Authorizations
defaultAuthorizations = Authorizations
    { createObjectAuthz = \_ _ -> [pure AllowR]
    , lookupObjectAuthz = \cred objId ->
        [ sufficient $ do
            session <- case cred of
                SessionIdCredential sId -> lookupSession sId
            sessionCreatedObject session objId
        ]
    , patchObjectAuthz = \cred objId _ ->
        [ sufficient $ do
            session <- case cred of
                SessionIdCredential sId -> lookupSession sId
            sessionCreatedObject session objId
        ]
    , deleteObjectAuthz = \cred objId ->
        [ sufficient $ do
            session <- case cred of
                SessionIdCredential sId -> lookupSession sId
            sessionCreatedObject session objId
        ]
    , uploadBlobAuthz = \_ _ -> [pure AllowR]
    , lookupBlobAuthz = \_ _ -> [pure AllowR]
    , lookupBlobContentAuthz = \_ _ -> [pure AllowR]
    }



--------------------------------------------------------------------------------
-- | Authorization logic is implemented as a list of 'Avers' actions, each of
-- which we call a @module@ and returns a result ('AuthzR'), which determines
-- what happens next.

type Authz = [Avers AuthzR]



--------------------------------------------------------------------------------
-- | The result of a single module is either 'ContinueR', which means we
-- continue executing following modules, 'AllowR' which means that the
-- action is allowed and any following modules are skipped, or 'RejcetR' which
-- means that the action is rejected and following modules are skipped as well.

data AuthzR = ContinueR | AllowR | RejectR




--------------------------------------------------------------------------------
-- | Run the authorization logic inside of the Servant monad.

runAuthorization :: Handle -> Authz -> ExceptT ServantErr IO ()
runAuthorization _      []     = pure ()
runAuthorization aversH (x:xs) = do
    res <- liftIO $ evalAvers aversH x
    case res of
        Left _  -> throwError err500
        Right r -> case r of
            ContinueR -> runAuthorization aversH xs
            AllowR    -> pure ()
            RejectR   -> throwError err401




--------------------------------------------------------------------------------
-- | This doesn't change the result, but allows you to run arbitrary 'Avers'
-- actions. This is useful for debugging.

trace :: Avers () -> Avers AuthzR
trace m = m >> pure ContinueR


--------------------------------------------------------------------------------
-- | If the given 'Avers' action returns 'True', it is sufficient to pass
-- the authorization check.

sufficient :: Avers Bool -> Avers AuthzR
sufficient m = do
    res <- m
    pure $ if res then AllowR else ContinueR


--------------------------------------------------------------------------------
-- | The given 'Avers' action must return 'True' for this authorization check
-- to pass.

requisite :: Avers Bool -> Avers AuthzR
requisite m = do
    res <- m
    pure $ if res then ContinueR else RejectR



------------------------------------------------------------------------------
-- Authorization modules
------------------------------------------------------------------------------

-- | True if the session created the given object.
sessionCreatedObject :: Session -> ObjId -> Avers Bool
sessionCreatedObject session objId = do
    obj <- Avers.lookupObject objId
    return $ objectCreatedBy obj == sessionObjId session

-- | True if the session is the given object. In most cases, a session has full
-- access to the object against which it was created.
sessionIsObject :: Session -> ObjId -> Avers Bool
sessionIsObject session objId = do
    return $ sessionObjId session == objId
