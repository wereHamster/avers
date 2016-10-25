{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}

module Avers.Views where


import           Control.Monad

import           Data.Text (Text)
import           Data.Aeson as Aeson
import           Data.Monoid
import qualified Data.HashMap.Strict as HMS

import qualified Database.RethinkDB as R

import           Avers.Types
import           Avers.Storage.Backend



viewTableName :: View obj a -> Text
viewTableName View{..} = "view_" <> viewName


-- | Construct the table name for the given view. The table names look
-- something like this: "view_openGames"
viewTable :: View obj a -> R.Exp R.Table
viewTable view = R.Table Nothing $ R.lift $ viewTableName view


data Record a = Record
    { recId      :: ObjId
    , recContent :: a
    }

instance Pk (Record a) where
    toPk = toPk . recId

instance (ToJSON a) => ToJSON (Record a) where
    toJSON Record{..} = Aeson.Object $ HMS.insert "id" (Aeson.String $ toPk recId) hms
      where (Aeson.Object hms) = toJSON recContent

instance (R.ToDatum a) => R.ToDatum (Record a) where
    toDatum Record{..} = R.Object $ HMS.insert "id" (R.String $ toPk recId) $
        HMS.fromList $ map (\(k,v) -> (k, R.toDatum v)) $ HMS.toList hms
      where (R.Object hms) = R.toDatum recContent



updateObjectViews :: ObjectType a -> ObjId -> Maybe a -> Avers ()
updateObjectViews ObjectType{..} objId mbObj = do
    forM_ otViews $ \(SomeView view) -> do
        updateView view objId mbObj


updateView :: (R.ToDatum a) => View obj a -> ObjId -> Maybe obj -> Avers ()
updateView view@View{..} objId mbObj = do
    case mbObj of
        Nothing  -> deleteDocument (viewTable view) objId
        Just obj -> do
            mbRecord <- viewObjectTransformer obj
            case mbRecord of
                Nothing  -> deleteDocument (viewTable view) objId
                Just rec -> upsertDocument (viewTable view) (Record objId rec)
