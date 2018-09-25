{-# LANGUAGE OverloadedStrings     #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

{-# LANGUAGE ScopedTypeVariables   #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Avers.Server.Instances where


import Data.Proxy
import Data.ByteString.Conversion
import qualified Data.Text.Encoding as T

import Avers
import Avers.API

import Servant.API
import Servant.Server
import Servant.Server.Internal

import Network.Wai

import Web.Cookie



instance (HasServer sublayout context) => HasServer (Credentials :> sublayout) context where

    type ServerT (Credentials :> sublayout) m =
        Credentials -> ServerT sublayout m

    route Proxy context subserver =
        route (Proxy :: Proxy sublayout) context (addAuthCheck subserver authCheck)
      where
        authCheck = withRequest $ \request -> do
            let mbCookieHeaders = lookup "cookie" (requestHeaders request)
                mbSessionIdText = lookup "session" =<< fmap parseCookiesText mbCookieHeaders

            case mbSessionIdText of
                Nothing -> pure CredAnonymous
                Just sessionIdText -> case parseQueryParam sessionIdText of
                    Left _ -> delayedFailFatal err400
                    Right sId -> pure $ CredSessionId $ SessionId sId

    hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy sublayout) pc nt . s


instance (HasServer sublayout context) => HasServer (SessionId :> sublayout) context where

    type ServerT (SessionId :> sublayout) m =
        SessionId -> ServerT sublayout m

    route Proxy context subserver =
        route (Proxy :: Proxy sublayout) context (addAuthCheck subserver authCheck)
      where
        authCheck = withRequest $ \request -> do
            let mbCookieHeaders = lookup "cookie" (requestHeaders request)
                mbSessionIdText = lookup "session" =<< fmap parseCookiesText mbCookieHeaders

            case mbSessionIdText of
                Nothing -> delayedFailFatal err401
                Just sessionIdText -> case parseQueryParam sessionIdText of
                    Left _ -> delayedFailFatal err401
                    Right sId -> pure $ SessionId sId

    hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy sublayout) pc nt . s


instance ToHttpApiData SetCookie where
    toUrlPiece = T.decodeUtf8 . toByteString' . renderSetCookie
