{-# LANGUAGE OverloadedStrings #-}

module PostgresqlSimpleSpec where

import Control.Concurrent
import Control.Concurrent.Async
import Data.Pool
import Database.PostgreSQL.Simple hiding (fold)
import Database.PostgreSQL.Simple.Notification
import Test.Syd
import TestUtils

spec :: Spec
spec = flaky 3 $ postgresSpec $ do
  describe "different connections" $ do
    it "needs a sleep" $ \pool -> withTimeout $ do
      withAsync (withResource pool $ \conn -> execute_ conn "LISTEN virtual" >> getNotification conn) $ \asyncNotification -> do
        threadDelay 100000
        _ <- withResource pool $ \conn -> execute_ conn "NOTIFY virtual, 'palayloada';"
        (Notification _ channel payload) <- wait asyncNotification
        channel `shouldBe` "virtual"
        payload `shouldBe` "palayloada"
    it "doesn't need a sleep" $ \pool -> withTimeout $ do
      var <- newEmptyMVar
      _ <- withResource pool $ \conn -> execute_ conn "LISTEN virtual" >> async (getNotification conn >>= putMVar var)
      _ <- withResource pool $ \conn -> execute_ conn "NOTIFY virtual, 'palayloada';"
      (Notification _ channel payload) <- takeMVar var
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
    it "listen, notify, getNotification (MVar)" $ \pool -> withTimeout $ do
      var <- newEmptyMVar
      withResource pool $ \conn -> do
        _ <- execute_ conn "LISTEN virtual"
        _ <- execute_ conn "NOTIFY virtual, 'palayloada'"
        _ <- getNotification conn >>= putMVar var
        (Notification _ channel payload) <- takeMVar var
        channel `shouldBe` "virtual"
        payload `shouldBe` "palayloada"
    it "listen, getNotification, notify (MVar)" $ \pool -> withTimeout $ do
      var <- newEmptyMVar
      withResource pool $ \conn -> do
        _ <- execute_ conn "LISTEN virtual"
        _ <- async $ getNotification conn >>= putMVar var
        _ <- execute_ conn "NOTIFY virtual, 'palayloada'"
        (Notification _ channel payload) <- takeMVar var
        channel `shouldBe` "virtual"
        payload `shouldBe` "palayloada"
