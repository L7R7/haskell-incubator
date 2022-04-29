{-# LANGUAGE OverloadedStrings #-}

module HasqlSpec where

import Control.Concurrent
import Control.Concurrent.Async
import Data.Pool
import Hasql.Notifications
import Test.Syd
import TestUtils

spec :: Spec
spec = flaky 3 $ sequential $ postgresHasqlSpec $ do
  describe "same connection" $ do
    it "works" $ \pool -> withTimeout $ do
      withResource pool $ \conn -> do
        var <- newEmptyMVar
        let channel = toPgIdentifier "foo"
        listen conn channel
        _ <- async $ waitForNotifications (notificationHandler var) conn
        res <- notify conn channel "hello"
        res `shouldBe` Right ()

        result <- takeMVar var
        result `shouldBe` ("foo", "hello")

  describe "different connections" $ do
    it "listen, waitForNotifications, notify" $ \pool -> withTimeout $ do
      var <- newEmptyMVar
      let channel = toPgIdentifier "foo"
      _ <- withResource pool $ \conn -> listen conn channel >> async (waitForNotifications (notificationHandler var) conn)
      res <- withResource pool $ \conn -> notify conn channel "hello"
      res `shouldBe` Right ()

      result <- takeMVar var
      result `shouldBe` ("foo", "hello")
    it "listen, notify, waitForNotifications" $ \pool -> withTimeout $ do
      var <- newEmptyMVar
      let channel = toPgIdentifier "foo"
      withResource pool $ \conn -> listen conn channel
      res <- withResource pool $ \conn -> notify conn channel "hello"
      res `shouldBe` Right ()
      _ <- withResource pool $ \conn -> async (waitForNotifications (notificationHandler var) conn)

      result <- takeMVar var
      result `shouldBe` ("foo", "hello")

notificationHandler :: MVar (a, b) -> a -> b -> IO ()
notificationHandler var channel payload = putMVar var (channel, payload)
