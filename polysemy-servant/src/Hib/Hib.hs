{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | servant + polysemy + html using lucid
module Hib.Hib
  ( logoutLink,
    someFunc,
    application,
  )
where

import Control.Monad.Trans.Except
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time
import qualified Hib.Cookies as C
import Lucid.Base
import Lucid.Html5
import Network.HTTP.Types (hLocation)
import qualified Network.Wai.Handler.Warp as Wai
import Polysemy
import Polysemy.Error (Error, runError, throw)
import Servant
import Servant.API.Generic
import Servant.Auth
import Servant.Auth.Server
import Servant.HTML.Lucid
import Servant.Server.Generic (AsServerT)
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

type API = NamedRoutes NamedAPI

data NamedAPI mode = NamedAPI
  { rootRedirect :: mode :- Verb 'GET 302 '[HTML] (Headers '[Header "Location" URI] String),
    protectedApi :: mode :- Auth '[Cookie] User :> "name" :> Get '[HTML] (Html ()),
    loginEndpoints :: mode :- "login" :> NamedRoutes LoginAPI,
    logoutEndpoints :: mode :- "logout" :> NamedRoutes LogoutAPI,
    raw :: mode :- "static" :> Raw
  }
  deriving (Generic)

data LoginAPI mode = LoginAPI
  { loginForm ::
      mode :- ReqBody '[FormUrlEncoded] Login
        :> Verb 'POST 302 '[WhyIsThisNotUnit] (Headers '[Header "Location" URI, Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] String),
    loginUi ::
      mode :- C.Cookie "sbLogin" ByteString
        :> QueryParam "ref" LoginRef
        :> Get '[HTML] (Html ())
  }
  deriving (Generic)

newtype LogoutAPI mode = LogoutAPI
  { logoutEndpoint :: mode :- Verb 'POST 302 '[JSON] (Headers '[Header "Location" URI, Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] String)
  }
  deriving (Generic)

loginFormLink :: Link
nameLink :: Link
loginLink :: Maybe LoginRef -> Link
logoutLink :: Link
(NamedAPI _ nameLink (LoginAPI loginFormLink loginLink) (LogoutAPI logoutLink) _) = allFieldLinks @NamedAPI

context :: CookieSettings -> JWTSettings -> Context '[CookieSettings, JWTSettings]
context cookieCfg jwtConfig = cookieCfg :. jwtConfig :. EmptyContext

hoist :: CookieSettings -> JWTSettings -> ServerT API Handler
hoist cookieSettings jwtSettings = hoistServerWithContext (Proxy @API) (Proxy @'[CookieSettings, JWTSettings]) liftServer (server cookieSettings jwtSettings)

instance FromHttpApiData ByteString where
  parseQueryParam = pure . encodeUtf8

liftServer :: Sem '[Error ServerError, Embed IO] a -> Handler a
liftServer sem =
  sem
    & runError @ServerError
    & runM
    & Handler . ExceptT

server :: CookieSettings -> JWTSettings -> NamedAPI (AsServerT (Sem '[Error ServerError, Embed IO]))
server cs js =
  NamedAPI
    { rootRedirect = redirectRoot,
      protectedApi = nameEndpoint,
      loginEndpoints =
        LoginAPI
          { loginForm = checkCreds cs js,
            loginUi = loginPage js
          },
      logoutEndpoints =
        LogoutAPI
          { logoutEndpoint = logout cs
          },
      raw = serveDirectoryWebApp "static"
    }

redirectRoot :: Sem r (Headers '[Header "Location" URI] String)
redirectRoot = pure $ addHeader (linkURI nameLink) "root-redirect"

loginPage ::
  ( Member (Error ServerError) r,
    Member (Embed IO) r
  ) =>
  JWTSettings ->
  Maybe (C.CookieVal "sbLogin" ByteString) ->
  Maybe LoginRef ->
  Sem r (Html ())
loginPage js authCookie maybeRef = do
  maybeUser <- case authCookie of
    (Just (C.CookieVal (Just bs))) -> embed $ verifyJWT @User js bs
    _ -> pure Nothing
  case maybeUser of
    Just _ -> throw err302 {errHeaders = [(hLocation, toHeader nameLink)]}
    Nothing -> pure $
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
  ( Member (Embed IO) r,
    Member (Error ServerError) r
  ) =>
  CookieSettings ->
  JWTSettings ->
  Login ->
  Sem r (Headers '[Header "Location" URI, Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] String)
checkCreds cookieSettings jwtSettings Login {username = "AliBaba", password = "OpenSesame"} = do
  mApplyCookies <- embed $ acceptLogin cookieSettings jwtSettings (User "Ali Baba" "foo")
  case mApplyCookies of
    Nothing -> throw err302 {errHeaders = [(hLocation, toHeader $ loginLink (Just BadCreds))]}
    Just applyCookies -> pure $ addHeader (linkURI nameLink) $ applyCookies "logged in"
checkCreds _ _ _ = throw $ err302 {errHeaders = [(hLocation, toHeader $ loginLink (Just BadCreds))]}

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
nameEndpoint _ = throw $ err302 {errHeaders = [(hLocation, toHeader $ loginLink (Just Denied))]}

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
            cookieMaxAge = Just (secondsToDiffTime 6000),
            cookieSameSite = SameSiteStrict,
            sessionCookieName = "sbLogin",
            cookieXsrfSetting = Just def {xsrfExcludeGet = True}
          }
  jwtSettings <- defaultJWTSettings <$> generateKey
  pure $ serveWithContext (Proxy @API) (context cookieConfig jwtSettings) (hoist cookieConfig jwtSettings)
