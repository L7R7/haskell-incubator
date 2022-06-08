{-# LANGUAGE OverloadedStrings #-}

module HibSpec where

import Data.ByteString.Char8 (pack)
import Data.Foldable (find)
import Hib.Hib (application, logoutLink)
import Servant
import Test.Syd
import Test.Syd.Wai

spec :: Spec
spec = waiClientSpecWith application $ do
  describe "authenticating" $ do
    describe "invalid login requests" $ do
      it "empty post returns 415" $ do
        resp <- post "/login" ""
        liftIO $ responseStatus resp `shouldBe` unsupportedMediaType415

      it "empty post with correct Content-Type returns 400" $ do
        resp <- request methodPost "/login" [("Content-Type", "application/x-www-form-urlencoded")] ""
        liftIO $ responseStatus resp `shouldBe` badRequest400

      it "post with correct Content-Type and invalid creds returns 401" $ do
        resp <- request methodPost "/login" [("Content-Type", "application/x-www-form-urlencoded")] "username=foo&password=wrong"
        liftIO $ do
          responseStatus resp `shouldBe` found302
          context "location header" $ snd <$> find (\h -> fst h == hLocation) (responseHeaders resp) `shouldBe` Just "login?ref=badcreds"

    describe "valid requests" $ do
      it "post with correct Content-Type and valid creds sets cookies and returns 302 with URL to /name" $ do
        resp <- request methodPost "/login" [("Content-Type", "application/x-www-form-urlencoded")] "username=AliBaba&password=OpenSesame"
        let cookies = destroyCookieJar $ responseCookieJar resp
        liftIO $ do
          context "number of cookies" $ length cookies `shouldBe` 2
          context "cookie names" $ cookie_name <$> cookies `shouldBe` ["XSRF-TOKEN", "sbLogin"]
          context "response status" $ responseStatus resp `shouldBe` found302
          context "location header" $ snd <$> find (\h -> fst h == hLocation) (responseHeaders resp) `shouldBe` Just "name"

        resp'' <- get "/name"
        liftIO $ context "resource can be accessed" $ responseStatus resp'' `shouldBe` ok200

      it "/fooo can be accessed after login" $ do
        resp <- request methodPost "/login" [("Content-Type", "application/x-www-form-urlencoded")] "username=AliBaba&password=OpenSesame"
        liftIO $ context "login response status" $ responseStatus resp `shouldBe` found302

        resp' <- get "/fooo"
        liftIO $ context "resource can be accessed" $ responseStatus resp' `shouldBe` ok200

  describe "logout" $ do
    let logoutUriString = pack $ show $ linkURI logoutLink
    it "post to logout returns 204 and sets cookies" $ do
      resp <- post logoutUriString ""
      let cookies = destroyCookieJar $ responseCookieJar resp
      liftIO $ do
        context "number of cookies" $ length cookies `shouldBe` 2
        context "cookie names" $ cookie_name <$> cookies `shouldBe` ["XSRF-TOKEN", "sbLogin"]
        context "cookie values" $ cookie_value <$> cookies `shouldBe` ["value", "value"]
        context "response status" $ responseStatus resp `shouldBe` found302
        context "location header" $ snd <$> find (\h -> fst h == hLocation) (responseHeaders resp) `shouldBe` Just "login?ref=loggedout"

    it "after logout, the /name endpoint can NOT be accessed" $ do
      resp <- request methodPost "/login" [("Content-Type", "application/x-www-form-urlencoded")] "username=AliBaba&password=OpenSesame"
      liftIO $
        context "authentication works" $ do
          responseStatus resp `shouldBe` found302
          snd <$> find (\h -> fst h == hLocation) (responseHeaders resp) `shouldBe` Just "name"
      resp' <- post logoutUriString ""
      liftIO $
        context "logout works" $ do
          responseStatus resp' `shouldBe` found302
          snd <$> find (\h -> fst h == hLocation) (responseHeaders resp') `shouldBe` Just "login?ref=loggedout"

      resp'' <- get "/name"
      liftIO $
        context "resource can't be accessed" $ do
          responseStatus resp'' `shouldBe` found302
          snd <$> find (\h -> fst h == hLocation) (responseHeaders resp'') `shouldBe` Just "login?ref=denied"

  describe "authenticated" $ do
    it "redirects when accessing login page" $ do
      resp <- request methodPost "/login" [("Content-Type", "application/x-www-form-urlencoded")] "username=AliBaba&password=OpenSesame"
      liftIO $
        context "authentication works" $ do
          responseStatus resp `shouldBe` found302
          snd <$> find (\h -> fst h == hLocation) (responseHeaders resp) `shouldBe` Just "name"

      resp' <- get "/login"
      liftIO $
        context "user is already authenticated and is redirected to the resource" $ do
          responseStatus resp' `shouldBe` found302
          snd <$> find (\h -> fst h == hLocation) (responseHeaders resp') `shouldBe` Just "name"

    it "if there is something in the login cookie but it's not a valid login, it shows the login page" $ do
      resp <- request methodGet "/login" [("Cookie", "sbLogin=imAHacker")] ""
      liftIO $ responseStatus resp `shouldBe` ok200

  describe "unauthenticated" $ do
    it "login page returns 200 with HTML" $ do
      resp <- get "/login"
      liftIO $ do
        responseStatus resp `shouldBe` ok200
        snd <$> find (\h -> fst h == hContentType) (responseHeaders resp) `shouldBe` Just "text/html;charset=utf-8"
    it "redirects to /name when accessing /" $ do
      resp <- get "/"
      liftIO $ do
        responseStatus resp `shouldBe` found302
        snd <$> find (\h -> fst h == hLocation) (responseHeaders resp) `shouldBe` Just "name"

    describe "unauthenticated access to protected resources redirects to login" $ do
      it "/name" $ do
        resp <- get "/name"
        liftIO $ do
          responseStatus resp `shouldBe` found302
          context "location header" $ snd <$> find (\h -> fst h == hLocation) (responseHeaders resp) `shouldBe` Just "login?ref=denied"

      it "/fooo" $ do
        resp <- get "/fooo"
        liftIO $ do
          responseStatus resp `shouldBe` found302
          context "location header" $ snd <$> find (\h -> fst h == hLocation) (responseHeaders resp) `shouldBe` Just "login?ref=denied"

    it "serves static files" $ do
      resp <- get "static/styles.css"
      liftIO $ do
        responseStatus resp `shouldBe` ok200
        responseBody resp `shouldBe` "/* test works */\n"
