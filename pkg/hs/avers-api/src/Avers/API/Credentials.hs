module Avers.API.Credentials where


import Avers



-- | Credentials are used to authenticate the client. It can be a SessionId
-- (extracted from the 'session' cookie).
--
-- Later we may add support for token based authentication. Then we extend
-- this into a sumtype covering all the possible ways how credentials can be
-- passed along with the request.

data Credentials
    = CredAnonymous
    | CredSessionId !SessionId
    -- AccessTokenCredential !AccessToken
