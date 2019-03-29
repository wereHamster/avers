{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Avers.API.Documentation
    ( swaggerAversAPI
    ) where


import           Control.Lens

import           Data.Proxy
import           Data.Swagger     hiding (Operation, Header)
import           Data.Aeson       hiding (Options(..))
import           Data.Text        (Text)
import qualified Data.Text        as Text
import           Data.Char

import           Servant.API      hiding (Patch)
import           Servant.Swagger

import           Web.Cookie

import           Avers.Types
import           Avers.API



-- | Add parameter to every operation in the spec.
addParam :: Param -> Swagger -> Swagger
addParam param = allOperations.parameters %~ (Inline param :)

addDefaultResponse400 :: ParamName -> Swagger -> Swagger
addDefaultResponse400 n = setResponseWith (\old _new -> alter400 old) 400 (pure response400)
  where
    description400 = "Invalid " <> n
    alter400 = description %~ (<> (" or " <> n))
    response400 = mempty & description .~ description400



--------------------------------------------------------------------------------
schemaOptions :: String -> SchemaOptions
schemaOptions p = defaultSchemaOptions
    { fieldLabelModifier     = dropPrefix p
    , constructorTagModifier = map toLower
    }

dropPrefix :: String -> String -> String
dropPrefix p x = toLower (head rest) : tail rest
  where rest = drop (length p) x



--------------------------------------------------------------------------------
instance (HasSwagger sub) => HasSwagger (Credentials :> sub) where
    toSwagger _ = toSwagger (Proxy :: Proxy sub)
        & addParam param
        & addDefaultResponse400 (Text.pack paramName)
        where
          paramName = "Cookie"
          param = mempty
            & name .~ (Text.pack paramName)
            & schema .~ ParamOther (mempty
                & in_ .~ ParamHeader
                & paramSchema .~ toParamSchema (Proxy :: Proxy Text))


--------------------------------------------------------------------------------
instance (HasSwagger sub) => HasSwagger (SessionId :> sub) where
    toSwagger _ = toSwagger (Proxy :: Proxy sub)
        & addParam param
        & addDefaultResponse400 (Text.pack paramName)
        where
          paramName = "Cookie"
          param = mempty
            & name .~ (Text.pack paramName)
            & schema .~ ParamOther (mempty
                & in_ .~ ParamHeader
                & paramSchema .~ toParamSchema (Proxy :: Proxy Text))



--------------------------------------------------------------------------------
instance ToSchema Value where
    declareNamedSchema _ = pure (NamedSchema (Just "Value") (mempty & type_ .~ SwaggerObject))


--------------------------------------------------------------------------------
instance ToParamSchema ObjectId where
    toParamSchema _ = toParamSchema (Proxy :: Proxy Text)

instance ToSchema ObjectId where
    declareNamedSchema a = pure (NamedSchema (Just "ObjectId") (paramSchemaToSchema a))


instance ToParamSchema ObjId
instance ToSchema ObjId
instance ToParamSchema RevId
instance ToSchema RevId
instance ToSchema SecretId
instance ToSchema SessionId
instance ToSchema BlobId
instance ToParamSchema BlobId
instance ToSchema Path


instance ToSchema BlobContent where
    declareNamedSchema _ = pure (NamedSchema (Just "BlobContent") binarySchema)


--------------------------------------------------------------------------------
-- FIXME: This doesn't generate a correct representation of an 'Operation'
-- object. May need to declare it manually :(
instance ToSchema Operation where
    declareNamedSchema = genericDeclareNamedSchema (schemaOptions "op")


--------------------------------------------------------------------------------
instance ToSchema Patch where
    declareNamedSchema = genericDeclareNamedSchema (schemaOptions "patch")



instance ToSchema CreateObjectBody where
    declareNamedSchema = genericDeclareNamedSchema (schemaOptions "cob")
instance ToSchema CreateObjectResponse where
    declareNamedSchema = genericDeclareNamedSchema (schemaOptions "cor")


instance ToSchema LookupObjectResponse where
    declareNamedSchema = genericDeclareNamedSchema (schemaOptions "lor")


instance ToSchema PatchObjectBody where
    declareNamedSchema = genericDeclareNamedSchema (schemaOptions "pob")
instance ToSchema PatchObjectResponse where
    declareNamedSchema = genericDeclareNamedSchema (schemaOptions "por")


instance ToSchema ObjectChangeNotification where
    declareNamedSchema = genericDeclareNamedSchema (schemaOptions "cob")


-- LookupPatchResponse is a type synonym for Patch
-- instance ToSchema LookupPatchResponse


instance ToSchema CreateReleaseBody where
    declareNamedSchema = genericDeclareNamedSchema (schemaOptions "crb")
instance ToSchema CreateReleaseResponse where
    declareNamedSchema = genericDeclareNamedSchema (schemaOptions "crr")


instance ToSchema LookupReleaseResponse where
    declareNamedSchema = genericDeclareNamedSchema (schemaOptions "lrr")

instance ToSchema LookupLatestReleaseResponse where
    declareNamedSchema = genericDeclareNamedSchema (schemaOptions "llrr")


instance ToSchema CreateSessionBody where
    declareNamedSchema = genericDeclareNamedSchema (schemaOptions "csb")
instance ToSchema CreateSessionResponse where
    declareNamedSchema = genericDeclareNamedSchema (schemaOptions "csr")


instance ToSchema LookupSessionResponse where
    declareNamedSchema = genericDeclareNamedSchema (schemaOptions "lsr")


instance ToSchema ChangeSecretBody where
    declareNamedSchema = genericDeclareNamedSchema (schemaOptions "csb")


instance ToSchema UploadBlobResponse where
    declareNamedSchema = genericDeclareNamedSchema (schemaOptions "")


instance ToSchema LookupBlobResponse where
    declareNamedSchema = genericDeclareNamedSchema (schemaOptions "")



--------------------------------------------------------------------------------

swaggerAversAPI :: Swagger
swaggerAversAPI = toSwagger (Proxy :: Proxy AversAPI)
  & info.title       .~ "Avers API"
  & info.version     .~ "0.0.1" -- FIXME: Get the version from the avers-api package
  & info.description ?~ "TODO"
  & info.license     ?~ License "MIT" (Just (URL "http://mit.com"))
