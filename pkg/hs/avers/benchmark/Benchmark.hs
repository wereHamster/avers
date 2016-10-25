{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main where


import           Control.Applicative
import           Control.Monad
import           Control.Monad.State

import           Data.Monoid
import           Data.IORef
import           Data.Maybe
import           Data.Text         (Text)
import qualified Data.Text         as T

import           System.Environment
import           System.IO.Unsafe

import           Network.URI

import           Criterion.Main

import           Avers
import           Avers.TH
import           Avers.Storage

import           Prelude


data Dummy = Dummy
    { dummyField :: Text
    } deriving (Show)

dummyObjectType :: ObjectType Dummy
dummyObjectType = ObjectType
    { otType   = "dummy"
    , otId     = ObjId <$> liftIO (newId 30)
    , otViews  = []
    }


databaseConfig :: IO URI
databaseConfig = do
    uri <- fromMaybe "//localhost/test" <$> lookupEnv "RETHINKDB"
    return $ fromJust $ parseRelativeReference uri


mkBenchmarkHandle :: IO Handle
mkBenchmarkHandle = do
    dbURI <- databaseConfig
    let config = Config dbURI undefined [SomeObjectType dummyObjectType] (\_ _ -> return ())
    Right h <- newState config
    return h


{-# NOINLINE handleRef #-}
handleRef :: IORef Handle
handleRef = unsafePerformIO $ do
    newIORef =<< mkBenchmarkHandle


benchAvers :: Avers a -> IO ()
benchAvers m = do
    h <- readIORef handleRef
    void $ evalAvers h m


bench_Storage :: ObjId -> Benchmark
bench_Storage objId
    = bgroup "Storage"
    [ bench_Storage_lookupObject objId
    , bench_Storage_newestSnapshot objId
    , bench_Storage_patchesAfterRevision objId
    , bench_Storage_lookupLatestSnapshot objId
    ]

bench_Storage_lookupObject :: ObjId -> Benchmark
bench_Storage_lookupObject objId
    = bench "lookupObject" $ nfIO $ benchAvers $ do
        Avers.Storage.lookupObject objId

bench_Storage_newestSnapshot :: ObjId -> Benchmark
bench_Storage_newestSnapshot objId
    = bench "newestSnapshot" $ nfIO $ benchAvers $ do
        newestSnapshot (BaseObjectId objId)

bench_Storage_patchesAfterRevision :: ObjId -> Benchmark
bench_Storage_patchesAfterRevision objId
    = bgroup "patchesAfterRevision"
    [ bench_Storage_patchesAfterRevision_empty objId
    , bench_Storage_patchesAfterRevision_partial objId
    , bench_Storage_patchesAfterRevision_all objId
    ]

bench_Storage_patchesAfterRevision_empty :: ObjId -> Benchmark
bench_Storage_patchesAfterRevision_empty objId
    = bench "empty" $ nfIO $ benchAvers $ do
        patchesAfterRevision (BaseObjectId objId) (RevId 9999)

bench_Storage_patchesAfterRevision_partial :: ObjId -> Benchmark
bench_Storage_patchesAfterRevision_partial objId
    = bench "partial" $ nfIO $ benchAvers $ do
        patchesAfterRevision (BaseObjectId objId) (RevId 50)

bench_Storage_patchesAfterRevision_all :: ObjId -> Benchmark
bench_Storage_patchesAfterRevision_all objId
    = bench "all" $ nfIO $ benchAvers $ do
        patchesAfterRevision (BaseObjectId objId) zeroRevId

bench_Storage_lookupLatestSnapshot :: ObjId -> Benchmark
bench_Storage_lookupLatestSnapshot objId
    = bench "lookupLatestSnapshot" $ nfIO $ benchAvers $ do
        lookupLatestSnapshot (BaseObjectId objId)



main :: IO ()
main = do
    h <- mkBenchmarkHandle
    Right objId <- evalAvers h $ do
        objId <- createObject dummyObjectType rootObjId (Dummy "dummy")
        forM_ [0..100] $ \(rev :: Int) -> do
            applyObjectUpdates
                (BaseObjectId objId)
                (RevId rev)
                rootObjId
                [Set (Path "field") (Just $ toJSON $ "dummy" <> T.pack (show rev))]
                False

        return objId

    putStrLn $ "Dummy object with id " ++ show objId
    Right res <- evalAvers h $ objectContent (BaseObjectId objId)
    print (res :: Dummy)

    defaultMain
        [ bench_Storage objId
        ]


deriveEncoding (deriveJSONOptions "dummy") ''Dummy
