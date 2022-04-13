{-# LANGUAGE LambdaCase #-}

module TestUtils (withTimeout, postgresSpec, postgresHasqlSpec) where

import Control.Concurrent
import Control.Concurrent.Async
import Data.ByteString hiding (putStrLn)
import Data.Pool
import Database.PostgreSQL.Simple hiding (fold)
import Database.Postgres.Temp
import qualified Hasql.Connection as HC
import Test.Syd

withTimeout :: IO b -> IO ()
withTimeout action = do
  result <- race (threadDelay (3 * 1000000)) action
  case result of
    Left _ -> expectationFailure "timed out"
    Right _ -> pure ()

postgresSpec :: TestDefM outers (Pool Connection) result -> TestDefM outers oldInner result
postgresSpec = setupAroundWith $ const setupFuncConnectionPool

setupFuncConnectionPool :: SetupFunc (Pool Connection)
setupFuncConnectionPool = SetupFunc withConnectionPool

withConnectionPool :: (Pool Connection -> IO r) -> IO r
withConnectionPool takeConnectionPool = do
  errorOrRes <- withDbCache $ \dbCache -> do
    let combinedConfig = defaultConfig <> cacheConfig dbCache -- <> verboseConfig
    withConfig combinedConfig $ \db -> withPool (toConnectionString db) takeConnectionPool
  failE errorOrRes

withPool :: ByteString -> (Pool Connection -> IO a) -> IO a
withPool connectionString f = createPool (connectPostgreSQL connectionString) close 2 60 10 >>= f

postgresHasqlSpec :: TestDefM outers (Pool HC.Connection) result -> TestDefM outers b result
postgresHasqlSpec = setupAroundWith $ const setupFuncConnectionPoolHasql

setupFuncConnectionPoolHasql :: SetupFunc (Pool HC.Connection)
setupFuncConnectionPoolHasql = SetupFunc withHasqlConnectionPool

withHasqlConnectionPool :: (Pool HC.Connection -> IO b) -> IO b
withHasqlConnectionPool takeConnectionPool = do
  errorOrRes <- withDbCache $ \dbCache -> do
    let combinedConfig = defaultConfig <> cacheConfig dbCache -- <> verboseConfig
    withConfig combinedConfig $ \db -> withHasqlPool (toConnectionString db) takeConnectionPool
  failE errorOrRes

withHasqlPool :: ByteString -> (Pool HC.Connection -> IO a) -> IO a
withHasqlPool connectionString f = createPool (HC.acquire connectionString >>= failE) HC.release 2 60 10 >>= f

failE :: Show e => Either e a -> IO a
failE = either (expectationFailure . show) pure