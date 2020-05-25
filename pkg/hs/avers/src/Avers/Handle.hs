{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Avers.Handle (newHandle, newState) where


import           Safe

import           Control.Monad
import           Control.Monad.Except

import           Control.Concurrent
import           Control.Concurrent.STM

import           Data.Maybe
import           Data.List (nub)

import qualified Data.Map  as M

import           Data.Pool

import           Data.Text (Text)
import qualified Data.Text as T

import           Network.URI

import qualified Database.RethinkDB as R

import           Avers.Types
import           Avers.Storage





newHandle :: Config -> IO (Either AversError Handle)
newHandle config = runExceptT $ do
    databaseName <- ExceptT $ pure $ extractDatabaseName config
    databaseHandlePool <- newDatabaseHandlePool config databaseName
    recentRevisionCache <- lift $ newTVarIO M.empty

    -- Ensure that ObjectType tags are unique.
    when (length (objectTypes config) /= length (nub $ map (\(SomeObjectType ot) -> otType ot) $ objectTypes config)) $
        throwError $ AversError "Object type tags are not unique"

    changeChan <- lift newBroadcastTChanIO
    lift $ void $ forkFinally
        (streamPatches databaseHandlePool changeChan)
        (const $ pure ())

    Handle
        <$> (pure config)
        <*> (pure databaseHandlePool)
        <*> (pure recentRevisionCache)
        <*> (pure changeChan)


newState :: Config -> IO (Either AversError Handle)
newState = newHandle
{-# DEPRECATED newState "Use 'newHandle' instead" #-}



newDatabaseHandlePool :: Config -> Text -> ExceptT AversError IO (Pool R.Handle)
newDatabaseHandlePool config db = do
    host <- ExceptT $ pure $ databaseHost config
    let port = databasePort config
    let mbAuth = databaseAuth config

    lift $ createPool (create host port mbAuth) destroy numStripes idleTime maxResources

  where
    create host port mbAuth = do
        putStrLn $ mconcat
            [ "Creating a new RethinkDB handle to "
            , T.unpack host
            , ":"
            , show port
            , " database "
            , T.unpack db
            ]

        R.newHandle host port mbAuth (R.Database (R.lift db))

    destroy handle = do
        putStrLn "Closing RethinkDB handle"
        R.close handle

    numStripes   = 1
    idleTime     = fromIntegral $ (60 * 60 :: Int)
    maxResources = 10


databaseHost :: Config -> Either AversError Text
databaseHost Config{..} = maybe (Left $ AversError "databaseHost: not given") Right $ do
    auth <- uriAuthority databaseURI
    return $ T.pack $ uriRegName auth

databasePort :: Config -> Int
databasePort Config{..} = fromMaybe R.defaultPort $ do
    auth <- uriAuthority databaseURI
    case uriPort auth of
        []  -> Nothing
        _:x -> readMay x

databaseAuth :: Config -> Maybe Text
databaseAuth Config{..} = do
    auth <- uriAuthority databaseURI
    return $ T.pack $ uriUserInfo auth

extractDatabaseName :: Config -> Either AversError Text
extractDatabaseName Config{..} = case tail $ uriPath $ databaseURI of
    "" -> Left $ AversError "databaseName: not given"
    db -> Right $ T.pack db
