{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}


{-|

Low-level implementation of the storage backend.

-}

module Avers.Storage.Backend
  ( parseValue
  , parseDatum

  , runQuery
  , runQueryDatum
  , runQuerySingleSelection
  , runQueryCollect

  , existsDocument
  , lookupDocument
  , insertDocument
  , upsertDocument
  , deleteDocument
  ) where


import           Prelude hiding (lookup)

import           Control.Monad.Except
import           Control.Monad.State

import           Data.Aeson (Value, Result(..))
import           Data.Aeson.Types (parse, parseEither)
import           Data.Pool
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HMS
import qualified Data.Vector as V

import qualified Database.RethinkDB       as R
import           Database.RethinkDB.TH

import           Avers.TH
import           Avers.Types
import           Avers.Storage.Expressions



parseValue :: (FromJSON a, MonadError AversError m) => Value -> m a
parseValue value = case parseEither parseJSON value of
    Left  e -> parseError value (T.pack e)
    Right x -> return x


parseDatum :: (R.FromDatum a, MonadError AversError m) => R.Datum -> m a
parseDatum datum = case parse R.parseDatum datum of
    Error e -> parseError (toJSON datum) (T.pack e)
    Success x -> return x



data WriteResponse = WriteResponse
  { writeResponseInserted  :: Int
  , writeResponseDeleted   :: Int
  , writeResponseReplaced  :: Int
  , writeResponseUnchanged :: Int
  , writeResponseSkipped   :: Int
  , writeResponseErrors    :: Int
  } deriving (Show)


checkWriteResponse :: Maybe R.Datum -> Avers ()
checkWriteResponse resp = do
    case resp of
        Nothing -> strErr "Error"
        Just x ->
            case parseEither R.parseDatum x of
                Left err -> strErr err
                Right WriteResponse{..} ->
                    if writeResponseErrors == 0
                        then return ()
                        else databaseError $ "Errors during write operation: " <> T.pack (show resp)

mergePk :: (R.ToDatum a, Pk a) => a -> R.Object
mergePk doc = HMS.insert primaryKeyField (R.String $ toPk doc) hms
  where (R.Object hms) = R.toDatum doc


runQuerySingleSelection :: (R.FromDatum a) => R.Exp R.SingleSelection -> Avers a
runQuerySingleSelection query = do
    res <- runQuery query
    case res of
        Nothing -> documentNotFound "..."
        Just x  -> parseDatum x

runQueryDatum :: (R.FromDatum a) => R.Exp R.Datum -> Avers a
runQueryDatum query = do
    res <- runQuery query
    parseDatum res

runQuery :: (R.FromResponse (R.Result a)) => R.Exp a -> Avers (R.Result a)
runQuery query = do
    pool <- gets hDatabaseHandlePool
    res <- liftIO $ withResource pool $ \handle -> do
        R.run handle query

    case res of
        Left e -> databaseError (T.pack $ show e)
        Right r -> return r

runQueryCollect :: (R.FromDatum a, R.Result e ~ R.Sequence a) => R.Exp e -> Avers (V.Vector a)
runQueryCollect query = do
    pool <- gets hDatabaseHandlePool
    res <- liftIO $ withResource pool $ \handle -> do
        r0 <- R.run handle query
        case r0 of
            Left e -> return $ Left e
            Right x -> R.collect handle x

    case res of
        Left e -> databaseError (T.pack $ show e)
        Right r -> return r



existsDocument :: (Pk k) => R.Exp R.Table -> k -> Avers Bool
existsDocument table key = do
    res <- runQuery $ R.IsEmpty $ R.Filter (primaryKeyEqE (toPk key)) table
    return $ not res


lookupDocument :: (Pk k, R.FromDatum a) => R.Exp R.Table -> k -> Avers (Maybe a)
lookupDocument table key = do
    res <- runQuery $ R.Get table (R.lift (toPk key))
    maybe (return Nothing) parseDatum res


insertDocument :: (R.ToDatum a, Pk a) => R.Exp R.Table -> a -> Avers ()
insertDocument table doc = do
    resp <- runQuery $ R.InsertObject R.CRError table (mergePk doc)
    checkWriteResponse $ Just $ R.Object resp


deleteDocument :: (Pk k) => R.Exp R.Table -> k -> Avers ()
deleteDocument table key = do
    resp <- runQuery $ R.Delete $ R.Get table (R.lift $ toPk key)
    checkWriteResponse $ Just $ R.Object resp


upsertDocument :: (R.ToDatum a, Pk a) => R.Exp R.Table -> a -> Avers ()
upsertDocument table doc = do
    resp <- runQuery $ R.InsertObject R.CRReplace table (mergePk doc)
    checkWriteResponse $ Just $ R.Object resp

$(deriveDatum (deriveJSONOptions "writeResponse") ''WriteResponse)
