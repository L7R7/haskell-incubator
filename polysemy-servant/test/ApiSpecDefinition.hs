{-# LANGUAGE OverloadedStrings #-}

module ApiSpecDefinition (apiSpec) where

import Network.HTTP.Client
import Network.HTTP.Types as HTTP
import Test.Syd
import Test.Syd.Wai

apiSpec :: TestDefM outers (WaiClient st) ()
apiSpec = do
  describe "no auth info" $
    it "name resource returns 401" $ do
      resp <- get "/name"
      liftIO $ responseStatus resp `shouldBe` unauthorized401

  describe "authenticating" $ do
    it "empty post returns 415" $ do
      resp <- post "/login" ""
      liftIO $ responseStatus resp `shouldBe` unsupportedMediaType415

    it "empty post with correct Content-Type returns 400" $ do
      resp <- request methodPost "/login" [("Content-Type", "application/x-www-form-urlencoded")] ""
      liftIO $ responseStatus resp `shouldBe` badRequest400

    it "post with correct Content-Type and invalid creds returns 401" $ do
      resp <- request methodPost "/login" [("Content-Type", "application/x-www-form-urlencoded")] "username=foo&password=wrong"
      liftIO $ responseStatus resp `shouldBe` unauthorized401

    it "post with correct Content-Type and valid creds returns 204 and sets cookies" $ do
      resp <- request methodPost "/login" [("Content-Type", "application/x-www-form-urlencoded")] "username=AliBaba&password=OpenSesame"
      let cookies = destroyCookieJar $ responseCookieJar resp
      liftIO $ do
        context "number of cookies" $ length cookies `shouldBe` 2
        context "cookie names" $ cookie_name <$> cookies `shouldBe` ["XSRF-TOKEN", "sbLogin"]
        context "response status" $ responseStatus resp `shouldBe` noContent204

    it "after authenticating, the /name endpoint can be accessed" $ do
      -- the cookieJar will be preserved between requests
      resp <- request methodPost "/login" [("Content-Type", "application/x-www-form-urlencoded")] "username=AliBaba&password=OpenSesame"
      liftIO $ context "authentication works" $ responseStatus resp `shouldBe` noContent204
      resp' <- get "/name"
      liftIO $ do
        responseStatus resp' `shouldBe` ok200
        responseBody resp' `shouldBe` "\"Ali Baba\""

  describe "logout" $ do
    it "post to logout returns 204 and sets cookies" $ do
      resp <- post "/logout" ""
      let cookies = destroyCookieJar $ responseCookieJar resp
      liftIO $ do
        context "number of cookies" $ length cookies `shouldBe` 2
        context "cookie names" $ cookie_name <$> cookies `shouldBe` ["XSRF-TOKEN", "sbLogin"]
        context "cookie values" $ cookie_value <$> cookies `shouldBe` ["value", "value"]
        context "response status" $ responseStatus resp `shouldBe` ok200

    it "after logout, the /name endpoint can NOT be accessed" $ do
      resp <- request methodPost "/login" [("Content-Type", "application/x-www-form-urlencoded")] "username=AliBaba&password=OpenSesame"
      liftIO $ context "authentication works" $ responseStatus resp `shouldBe` noContent204

      resp' <- post "/logout" ""
      liftIO $ context "logout works" $ responseStatus resp' `shouldBe` ok200

      resp'' <- get "/name"
      liftIO $ context "resource can't be accessed" $ responseStatus resp'' `shouldBe` unauthorized401
