{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | servant + polysemy + html using lucid
module Hib.Hib
  ( API,
    Login (..),
    LoginRef (..),
    api,
    someFunc,
    application,
  )
where

import Control.Monad.Trans.Except
import Data.Aeson (FromJSON, ToJSON)
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.Text as T
import Data.Time
import Debug.Trace (trace)
import Lucid.Base
import Lucid.Html5
import Network.HTTP.Types (hLocation)
import qualified Network.Wai.Handler.Warp as Wai
import Polysemy
import Polysemy.Error (Error, runError, throw)
import Servant
import Servant.API.Flatten
import Servant.API.Generic (Generic)
import Servant.Auth
import Servant.Auth.Server
import Servant.HTML.Lucid
import Web.FormUrlEncoded

someFunc :: IO ()
someFunc = startServer

data User = User {name :: String, email :: String}
  deriving (Eq, Show, Generic)

instance ToJSON User

instance ToJWT User

instance FromJSON User

instance FromJWT User

data Login = Login {username :: String, password :: String}
  deriving (Eq, Show, Generic)

instance FromForm Login

type WhyIsThisNotUnit = JSON

data LoginRef = LoggedOut | Denied | BadCreds

instance FromHttpApiData LoginRef where
  parseQueryParam "loggedout" = Right LoggedOut
  parseQueryParam "denied" = Right Denied
  parseQueryParam "badcreds" = Right BadCreds
  parseQueryParam _ = Left "can't parse LoginRef"

instance ToHttpApiData LoginRef where
  toQueryParam LoggedOut = "loggedout"
  toQueryParam Denied = "denied"
  toQueryParam BadCreds = "badcreds"

instance ToHttpApiData URI where
  toUrlPiece = T.pack . show

type LoginAPI =
  "login"
    :> ( ReqBody '[FormUrlEncoded] Login :> Verb 'POST 302 '[WhyIsThisNotUnit] (Headers '[Header "Location" URI, Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] String)
           :<|> QueryParam "ref" LoginRef :> Get '[HTML] (Html ())
       )

type LogoutAPI = "logout" :> Verb 'POST 302 '[JSON] (Headers '[Header "Location" URI, Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] String)

type Protected = Auth '[Cookie] User :> "name" :> Get '[HTML] (Html ())

type RootRedirect = Verb 'GET 302 '[HTML] (Headers '[Header "Location" URI] String)

type API =
  RootRedirect
    :<|> Protected
    :<|> LoginAPI
    :<|> LogoutAPI
    :<|> "static" :> Raw

api :: Proxy API
api = Proxy

loginFormLink :: Link
nameLink :: Link
loginLink :: Maybe LoginRef -> Link
logoutLink :: Link
_ :<|> nameLink :<|> (loginFormLink :<|> loginLink :<|> logoutLink :<|> _) = allLinks (Proxy :: Proxy (Flat API))

context :: CookieSettings -> JWTSettings -> Context '[CookieSettings, JWTSettings]
context cookieCfg jwtConfig = cookieCfg :. jwtConfig :. EmptyContext

hoist :: CookieSettings -> JWTSettings -> ServerT API Handler
hoist cookieSettings jwtSettings = hoistServerWithContext api (Proxy :: Proxy '[CookieSettings, JWTSettings]) liftServer (server cookieSettings jwtSettings)

liftServer :: Sem '[Error ServerError, Embed IO] a -> Handler a
liftServer sem =
  sem
    & runError @ServerError
    & runM
    & Handler . ExceptT

server :: (Member (Embed IO) r, Member (Error ServerError) r) => CookieSettings -> JWTSettings -> ServerT API (Sem r)
server cs js = redirectRoot :<|> nameEndpoint :<|> (checkCreds cs js :<|> loginPage) :<|> logout cs :<|> serveDirectoryWebApp "static"

redirectRoot :: Sem r (Headers '[Header "Location" URI] String)
redirectRoot = pure $ addHeader (linkURI nameLink) "root-redirect"

loginPage :: Maybe LoginRef -> Sem r (Html ())
loginPage maybeRef = pure $
  html_ $ do
    head_ $ do
      title_ "Login"
    body_ $ do
      h1_ "Login"
      refInfo
      form_ [action_ (toUrlPiece loginFormLink), method_ "POST"] $ do
        div_ $ do
          label_ [for_ "username"] "Username"
          input_ [type_ "username", name_ "username"]
        div_ $ do
          label_ [for_ "password"] "Passwort"
          input_ [type_ "password", name_ "password"]
        div_ $ do
          input_ [type_ "submit", name_ "submit"]
  where
    refInfo =
      traverse_
        ( p_ . \case
            LoggedOut -> "You've been logged out"
            Denied -> "login to get access to this resouce"
            BadCreds -> "the provided credentials are wrong or unknown"
        )
        maybeRef

checkCreds ::
  (Member (Embed IO) r, Member (Error ServerError) r) =>
  CookieSettings ->
  JWTSettings ->
  Login ->
  Sem r (Headers '[Header "Location" URI, Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] String)
checkCreds cookieSettings jwtSettings Login {username = "AliBaba", password = "OpenSesame"} = do
  mApplyCookies <- embed $ acceptLogin cookieSettings jwtSettings (User "Ali Baba" "foo")
  case mApplyCookies of
    Nothing -> throw err302 {errHeaders = [(hLocation, toHeader $ loginLink (Just BadCreds))]}
    Just applyCookies -> pure $ addHeader (linkURI nameLink) $ applyCookies "logged in"
checkCreds _ _ Login {username = user} = trace ("Received " ++ user) $ throw $ err302 {errHeaders = [(hLocation, toHeader $ loginLink (Just BadCreds))]}

logout :: CookieSettings -> Sem r (Headers '[Header "Location" URI, Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] [Char])
logout cookieSettings = pure $ addHeader (linkURI $ loginLink (Just LoggedOut)) $ clearSession cookieSettings "logged out"

nameEndpoint :: (Member (Error ServerError) r) => AuthResult User -> Sem r (Html ())
nameEndpoint (Authenticated (User n _)) = pure $
  html_ $ do
    head_ $ do
      title_ "Welcome"
    body_ $ do
      h1_ "Welcome back!"
      p_ ("Your name is " <> toHtml n)
      form_ [action_ (toUrlPiece logoutLink), method_ "POST"] $ input_ [type_ "submit", name_ "logout", value_ "logout"]
nameEndpoint x = trace ("Access Denied " ++ show x) $ throw $ err302 {errHeaders = [(hLocation, toHeader $ loginLink (Just Denied))]}

startServer :: IO ()
startServer = do
  let port = 8080
  putStrLn $ "Serving endpoint " ++ show port
  app <- application
  Wai.run port app

application :: IO Application
application = do
  let cookieConfig =
        defaultCookieSettings
          { cookieIsSecure = NotSecure,
            cookieMaxAge = Just (secondsToDiffTime 60),
            cookieSameSite = SameSiteStrict,
            sessionCookieName = "sbLogin",
            cookieXsrfSetting = Just def {xsrfExcludeGet = True}
          }
  jwtSettings <- defaultJWTSettings <$> generateKey
  pure $ serveWithContext api (context cookieConfig jwtSettings) (hoist cookieConfig jwtSettings)
