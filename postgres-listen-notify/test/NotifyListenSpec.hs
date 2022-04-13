{-# LANGUAGE OverloadedStrings #-}

module NotifyListenSpec where

import Control.Concurrent
import Control.Concurrent.Async
import Data.ByteString hiding (putStrLn)
import Data.Pool
import Database.PostgreSQL.Simple hiding (fold)
import Database.PostgreSQL.Simple.Notification
import Database.Postgres.Temp
import Test.Syd

spec :: Spec
spec = postgresSpec $ do
  describe "different connections" $ do
    it "needs a sleep" $ \pool -> withTimeout $ do
      withAsync (withResource pool $ \conn -> execute_ conn "LISTEN virtual" >> getNotification conn) $ \asyncNotification -> do
        threadDelay 100000
        _ <- withResource pool $ \conn -> execute_ conn "NOTIFY virtual, 'palayloada';"
        (Notification _ channel payload) <- wait asyncNotification
        channel `shouldBe` "virtual"
        payload `shouldBe` "palayloada"
  describe "same connection" $ do
    it "listen, notify, getNotification" $ \pool -> withTimeout $ do
      withResource pool $ \conn -> do
        _ <- execute_ conn "LISTEN virtual"
        _ <- execute_ conn "NOTIFY virtual, 'palayloada'"
        (Notification _ channel payload) <- getNotification conn
        channel `shouldBe` "virtual"
        payload `shouldBe` "palayloada"
    it "listen, getNotification, notify" $ \pool -> withTimeout $ do
      withResource pool $ \conn -> do
        _ <- execute_ conn "LISTEN virtual"
        withAsync (getNotification conn) $ \asyncNotification -> do
          _ <- execute_ conn "NOTIFY virtual, 'palayloada'"
          (Notification _ channel payload) <- wait asyncNotification
          channel `shouldBe` "virtual"
          payload `shouldBe` "palayloada"

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

failE :: Show e => Either e a -> IO a
failE = either (expectationFailure . show) pure

withPool :: ByteString -> (Pool Connection -> IO a) -> IO a
withPool connectionString f = createPool (connectPostgreSQL connectionString) close 2 60 10 >>= f
