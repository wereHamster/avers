{-# OPTIONS_GHC -fno-warn-orphans #-}

module Avers.API.Instances where


import Avers
import Web.HttpApiData (FromHttpApiData(..))



instance FromHttpApiData ObjId where
    parseUrlPiece = Right . ObjId

instance FromHttpApiData RevId where
    parseUrlPiece x = parseUrlPiece x >>= Right . RevId

instance FromHttpApiData BlobId where
    parseUrlPiece = Right . BlobId
